{-# LANGUAGE NamedFieldPuns #-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Db
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.GHC
import Distribution.Simple.Utils
import Distribution.Simple.Build
import Distribution.Simple.Install
import Distribution.Simple.Configure
import Distribution.PackageDescription
import System.FilePath

import System.IO

main = defaultMainWithHooks $ simpleUserHooks {
         buildHook = myBuildHook,
         preBuild  = myPreBuild,
         instHook  = myInst,
         copyHook  = myCopy
       }

-- Set loader buildability to false since we're building that ourselfs
myPreBuild args flags = return $ buildableHBI "rts-loader" False

myBuildHook pd lbi uh flags = do
  buildLoaderExecutable pd lbi flags
  -- gah Cabal throws an error if there are no buildable components
  unless (null $ allBuildInfo pd) $
    (buildHook simpleUserHooks) pd lbi uh flags

buildLoaderExecutable pd lbi flags = do
  let distPref  = fromFlag (buildDistPref flags)
      verbosity = fromFlag (buildVerbosity flags)

  (ghc, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  (gcc, _) <- requireProgram verbosity gccProgram (withPrograms lbi)

  ghcInfo <- getGhcInfo verbosity ghc

  let [Executable {exeName=exeName', modulePath}] =
        filter ((== "rts-loader") . exeName) $ executables pd
      targetDir = buildDir lbi </> exeName'
      exeDir    = targetDir </> (exeName' ++ "-tmp")

  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True exeDir

  let Just libDir = lookup "LibDir" ghcInfo
      includes = [libDir </> "include"]
      includeOpts = map ("-I"++) includes
      libs = ["dl"]
      libOpts = map ("-l"++) libs
      opts = [ "-std=gnu99"
             , "-o", targetDir </> exeName'
             ]
             ++ includeOpts
             ++ libOpts
             ++ [modulePath]

      gccInvokation = programInvocation gcc opts

  runProgramInvocation verbosity gccInvokation


myInst = myInstOrCopy instHook installVerbosity
myCopy = myInstOrCopy copyHook copyVerbosity

myInstOrCopy :: (UserHooks
                -> PackageDescription
                -> LocalBuildInfo
                -> UserHooks
                -> flags
                -> IO ())
             -> (flags -> Distribution.Simple.Setup.Flag Verbosity)
             -> PackageDescription
             -> LocalBuildInfo
             -> UserHooks
             -> flags
             -> IO ()
myInstOrCopy hook verbosity pd lbi hooks flags = do
    let [exe] = filter ((=="rts-loader") . exeName) (executables pd)
        Just install_dir =
            lookup "x-install-dir" $ customFieldsBI $ buildInfo exe

        libexec = fromPathTemplate $ libexecdir (installDirTemplates lbi)
        install_dir' = toPathTemplate $ substLibexecdir libexec install_dir

        lbi_libexec = modifyInstallDirTemplates (setBindir install_dir') lbi

        hbi_only_exe = disableAllInstallComponentsExcept pd "rts-loader"
        hbi_not_exe  = buildableHBI "rts-loader" False

        pd_only_exe = updatePackageDescription hbi_only_exe pd
        pd_not_exe  = updatePackageDescription hbi_not_exe pd

    distPref <- findDistPrefOrDefault (copyDistPref defaultCopyFlags)

    let copyFlags = defaultCopyFlags { copyDistPref = toFlag distPref
                                     , copyVerbosity = verbosity flags
                                     }

    install pd_only_exe lbi_libexec copyFlags

    (hook simpleUserHooks) pd_not_exe lbi hooks flags
 where
   modifyInstallDirTemplates f lbi =
       lbi { installDirTemplates = f (installDirTemplates lbi) }
   setBindir bindir' inst_dirs =
       inst_dirs { bindir = bindir' }

   substLibexecdirComp libexec "$libexecdir" = libexec
   substLibexecdirComp _ comp = comp

   substLibexecdir libexec = onSplitPath $
       map (substLibexecdirComp libexec . dropTrailingPathSeparator)


   onSplitPath f p  = joinPath $ f (splitPath p)


buildableHBI pkg build =
    (Nothing, [(pkg, setBuildable build)])

setBuildable build = emptyBuildInfo { buildable = build }

disableAllInstallComponentsExcept pd except =
    let lib  = (const (setBuildable False)) <$> library pd
        exes = [ (exeName exe, setBuildable (exeName exe == except))
               | exe <- executables pd
               ]
    in
      (lib, exes)
