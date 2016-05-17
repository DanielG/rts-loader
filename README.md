Haskell dynamic RTS loader
==========================

    Usage: rts-loader MAIN_CLOSURE [PRELOAD_LIBS..] libHSrts-*.so* libHSbase-*.so* [LIBS..] -- [HS_ARGV..]

The idea is to compile (and distribute) Haskell executables as shared libraries
and have this program load and execute them at runtime.

Integration with ghc-pkg to find library dependencies automatically will be
included later.

Example
-------

```
$ ./rts-loader ZCMain_main_closure $(ghc --print-libdir)/rts/libHSrts-ghc$(ghc --numeric-version).so $(ghc --print-libdir)/base_*/libHSbase-*-*-ghc$(ghc --numeric-version).so $PWD/libmain.so $PWD/libmain.so -- Main
"main"
$ ./rts-loader Main_notzumain_closure $(ghc --print-libdir)/rts/libHSrts-ghc$(ghc --numeric-version).so $(ghc --print-libdir)/base_*/libHSbase-*-*-ghc$(ghc --numeric-version).so $PWD/libmain.so $PWD/libmain.so -- Main
"not main"
```