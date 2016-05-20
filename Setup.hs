{-# LANGUAGE NamedFieldPuns #-}
import Control.Applicative
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Db
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.GHC
import Distribution.Simple.Utils
import Distribution.PackageDescription
import System.FilePath

main = defaultMainWithHooks $ simpleUserHooks {
         buildHook = myBuildHook
       }

myBuildHook pd lbi _uh flags = do
  let distPref  = fromFlag (buildDistPref flags)
      verbosity = fromFlag (buildVerbosity flags)

  (ghc, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  (gcc, _) <- requireProgram verbosity gccProgram (withPrograms lbi)

  ghcInfo <- getGhcInfo verbosity ghc

  withExeLBI pd lbi $ \Executable { exeName, modulePath } clbi -> do
    let targetDir = componentBuildDir lbi clbi
        exeDir    = targetDir </> (exeName ++ "-tmp")
    createDirectoryIfMissingVerbose verbosity True targetDir
    createDirectoryIfMissingVerbose verbosity True exeDir

    let Just libDir = lookup "LibDir" ghcInfo
    let includes = [libDir </> "include"]
        includeOpts = map ("-I"++) includes
        libs = ["dl"]
        libOpts = map ("-l"++) libs
        opts = ["-o", targetDir </> exeName]
               ++ includeOpts
               ++ libOpts
               ++ [modulePath]

        gccInvokation = programInvocation gcc opts

    runProgramInvocation verbosity gccInvokation
