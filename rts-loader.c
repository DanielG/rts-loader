#define _GNU_SOURCE
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <dlfcn.h>
#include <unistd.h>

#include <Rts.h>

#include "log.h"

#if defined(__gnu_linux__)
#define LD_PRELOAD "LD_PRELOAD"
#elif defined(__APPLE__)
#define LD_PRELOAD "DYLD_INSERT_LIBRARIES"
#else
#error "unsupported OS"
#endif

bool is_basename_prefix_of(const char *pfx, const char *path)
{
        // GNU basename "never modifies its argument"
        return strncmp(pfx, basename(path), strlen(pfx)) == 0;
}

bool is_rtslib(const char *path)
{
        return is_basename_prefix_of("libHSrts", path);
}

bool is_baselib(const char *path)
{
        return is_basename_prefix_of("libHSbase", path);
}

void usage(char *progname)
{
        printf("Usage: %s MAIN_CLOSURE [PRELOAD_LIBS..] libHSrts-*.so* libHSbase-*.so* [LIBS..] -- [HS_ARGV..]\n", progname);
}

int preloader(int argc, char *argv[])
{
        if(argc < 2) {
                usage(argv[0]);
                return 1;
        }

        size_t preload_blen = 128;
        size_t preload_slen = 0;
        char* preload = calloc(preload_blen, 1);
        char *preload_ptr = preload;
        if(!preload)
                log_error(return 1, "malloc() failed");

        char *rts = NULL;
        char *base = NULL;

        int i = 2;
        for(; i < argc; i++) {
                char *path = argv[i];
                log_debug("path: %s", path);

                if(preload_slen + strlen(path) + strlen(" ") + 1 > preload_blen)
                {
                        preload_blen *= 2;
                        preload = realloc(preload, preload_blen);
                        preload_ptr = strchr(preload, '\0');
                }

                int nchar = snprintf(preload_ptr, preload_blen - preload_slen, "%s ", path);
                if(nchar < 0)
                        log_errno(return 1, "snprintf() failed");
                preload_slen += nchar;
                preload_ptr  += nchar;
                assert(strlen(preload_ptr) < preload_blen);
                assert(preload_slen < preload_blen);

                if(is_rtslib(path)) {
                        rts = path;
                } else if(is_baselib(path)) {
                        base = path;
                }

                if(rts && base) {
                        break;
                }
        }

        const char *old_preload = getenv(LD_PRELOAD) ? getenv(LD_PRELOAD) : "";
        size_t final_preload_len = strlen(old_preload) + 1 + strlen(preload);
        char *final_preload = malloc(final_preload_len);
        if(!final_preload)
                log_error(return 1, "malloc() failed");

        int nchar = snprintf(final_preload, final_preload_len, "%s %s", old_preload, preload);
        if(nchar < 0)
                log_errno(return 1, "snprintf() failed");


        log_debug(LD_PRELOAD"=\"%s\"", final_preload);

        setenv("OLD_" LD_PRELOAD, old_preload, true);
        setenv(LD_PRELOAD, final_preload, true);
        free(final_preload);

        if(strlen(argv[0]) < 1)
                abort();

        char *argv0 = strdup(argv[0]);

        basename(argv[0])[0] = '=';

        int rv = execvp(argv0, argv);
        if(rv < 0)
                log_errno(return 1, "execvp() failed %d", errno);

        abort();
}

int loader(int argc, char *argv[], char ***hs_argv)
{
        if(argc < 2)
                abort();

        setenv(LD_PRELOAD, getenv("OLD_" LD_PRELOAD) ? getenv("OLD_" LD_PRELOAD) : "", true);
        unsetenv("OLD_" LD_PRELOAD);

        char *rts = NULL;
        char *base = NULL;

        int i=1;
        for(; i < argc; i++) {
                char *path = argv[i];
                if(is_rtslib(path)) {
                        rts = path;
                } else if(is_baselib(path)) {
                        base = path;
                } else
                        continue;

                if(rts && base)
                        break;
        }

        for(i++; i < argc; i++) {
                char *path = argv[i];

                if(strcmp(path, "--") == 0) {
                        *hs_argv = &argv[i+1];
                        return 0;
                }

                log_debug("opening %s", path);
                void* addr = dlopen(path, RTLD_GLOBAL | RTLD_LAZY);
                if(!addr)
                        log_error(return 1, "dlopen(%s) failed: %s", path, dlerror());
        }

        return 1;
}

typedef int (*rts_main_t)( int argc, char *argv[], StgClosure *main_closure, RtsConfig rts_config);
int main(int argc, char *argv[])
{
        if(argc < 1)
                abort();

        char* progbase = basename(argv[0]);

        if(strncmp(progbase, "=", 1) != 0) {
                return preloader(argc, argv);
        }

        if(argc < 2) {
                usage(argv[0]);
                return 1;
        }

        char *main_symbol = argv[1];
        char **hs_argv = NULL;
        int rv = loader(argc, argv, &hs_argv);
        if(rv != 0)
                log_error(return rv, "loading libraries failed");

        int hs_argc = 0;

        for(char **tmp = hs_argv; *tmp; tmp++) {
                hs_argc++;
                log_debug("hs_argv: %s", *tmp);
        }

        log_debug("hs_argc: %d", hs_argc);

        void *defRtsCfg_p = dlsym(RTLD_DEFAULT, "defaultRtsConfig");
        if(!defRtsCfg_p)
                log_error(return 1, "\"defaultRtsConfig\" symbol not found");

        StgClosure *main_closure =
                (StgClosure*) dlsym(RTLD_DEFAULT, main_symbol);
        if(!main_closure)
                log_error(return 1, "main closure (%s) not found", main_symbol);

        log_debug("main_symbol: %s %p", main_symbol, main_closure);

        RtsConfig __conf;
        memcpy(&__conf, defRtsCfg_p, sizeof(__conf));
        __conf.rts_opts_enabled = RtsOptsSafeOnly;
        __conf.rts_hs_main = rtsTrue;

        rts_main_t hs_main_ = dlsym(RTLD_DEFAULT, "hs_main");
        if(!hs_main_)
                log_error(return 1, "hs_main symbol not found");

        log_debug("executing hs_main");
        return hs_main_(hs_argc, hs_argv, main_closure,__conf);

        return 0;
}
