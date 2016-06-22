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
