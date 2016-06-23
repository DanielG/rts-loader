Haskell dynamic library loader
==============================

Load and execute functions from Haskell dynamic libraries without being
restricted to a single RTS/GHC version.

This package consists of the initial loader executable `rts-loader` written
in plain C and a Haskell library providing a convinient interface to it. The
initial loader executable is implemented in C since writing it in Haskell would
mean linking against the RTS which means we can't load libraries compiled with
another version of GHC.

In order to get Cabal to compile a pure C executable while not linking against
the RTS still some hacks in `Setup.hs` are employed to call `gcc` directly so
beware.

Usage
-----

The example below will locate the package "foopkg" in the package databases
configured for the Cabal project in the current directory and execute the
function "main" from the module "Lib". The package can be found in
`tests/lib` in the rts-loader source tree.

To run the example, execute the following commands in the rts-loader source
tree.

```
$ cabal install --only-dependencies && cabal configure && cabal build && cabal install tests/lib
$ rts_loader_libexecdir=dist/build/rts-loader dist/build/rts-loader-example/rts-loader-example
```

`rts_loader_libexecdir=...` is only required if you didn't install rts-loader
into a sandbox or elsewhere already.

    import System.Loader.RTS
    import System.Process
   
    import Distribution.Verbosity
    import Distribution.Package
    import Distribution.Simple.Configure
   
    import Distribution.Simple.LocalBuildInfo
    import Distribution.Simple.Configure (getPersistBuildConfig)
   
    main :: IO ()
    main = do
      lbi <- getPersistBuildConfig "dist"
   
      let verbosity = normal
          comp = compiler lbi
          pkg_db_stack = withPackageDB lbi
          prog_conf = withPrograms lbi
   
      pidx <- getInstalledPackages verbosity comp pkg_db_stack prog_conf
   
      loader_exe <- loaderExecutablePath
      loader_args <- loaderInvocation (SymbolIdentifier "Lib" "main") [] <$>
        (resolveLibraryInfo comp [WayDyn] $
          lookupLibraryInfo pidx $
            PQName $ PackageName "foopkg")
   
      print =<< rawSystem loader_exe loader_args
 
We use information written by `cabal configure` (see `getPersistBuildConfig`)
for convinience only this is not actually required to use the loader.
