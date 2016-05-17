#include <stdio.h>
#include <errno.h>
#include <string.h>

#define LOG_INFO "<I>"
#define LOG_ERR "<E>"

#define log(fmt,...) fprintf(stderr, "[%s] " fmt "\n", __func__, ##__VA_ARGS__)

#define log_debug log

#define log_errno(go, fmt, ...) ({                                  \
            char err[255];                                          \
            strerror_r(errno, err, sizeof(err)-1);                  \
                                                                    \
            log_error(go, fmt ": %s" , ##__VA_ARGS__, err);         \
        })

#define log_error(go, fmt, ...) ({                  \
            log(LOG_ERR " " fmt, ##__VA_ARGS__);    \
            go;                                     \
        })                                          \


#define log_gai(go, rv, fmt, ...) ({                             \
            if(rv == EAI_SYSTEM)                                 \
                log_errno(go, "[gai] " fmt, ##__VA_ARGS__);      \
              else                                               \
                log(LOG_ERR fmt "%s\n", gai_strerror(rv), ##__VA_ARGS__); \
        })
