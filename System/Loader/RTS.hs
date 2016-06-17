module System.Loader.RTS
    ( module System.Loader.RTS
    , module GHC.Way
    , Verbosity, silent, normal, verbose, deafening
    , CompilerId(..)
    , CompilerFlavor(..)
    , Version(..)
    , PackageDB(..)
    ) where

import Control.Monad
import Data.Version
import Data.Maybe
import Data.List
import Data.Ord
import Text.Encoding.Z
import System.Info
import System.FilePath
import System.Directory
import System.Posix.Process

import GHC.Way

import Distribution.Text
import Distribution.Package hiding (installedUnitId)
import Distribution.Compiler
import Distribution.Verbosity
import Distribution.Simple.Compiler
import Distribution.Simple.Configure
import Distribution.Simple.Program
import Distribution.Simple.PackageIndex
import Distribution.Simple.GHC as GHC (getGhcInfo, configure)
import Distribution.InstalledPackageInfo

import qualified Distribution.Simple.Program.HcPkg as HcPkg

import Paths_rts_loader

data PackageQualifier = PQName PackageName
                      | PQId PackageId
                      | PQUnitId UnitId

data SymbolIdentifier = SymbolIdentifier ModuleName OccName

sortByVersion :: [(Version, [a])] -> [(Version, [a])]
sortByVersion = sortBy comp
 where
   comp (a,_) (b, _) = a `compare` b

lookupPackageQualifier :: PackageIndex a -> PackageQualifier -> Maybe a
lookupPackageQualifier pidx (PQName pkg_name) = do
    (_ver, pkgs) <- listToMaybe $ reverse $ sortByVersion $
                      lookupPackageName pidx pkg_name
    listToMaybe pkgs
lookupPackageQualifier pidx (PQId pkg_id) =
    listToMaybe $ lookupSourcePackageId pidx pkg_id
lookupPackageQualifier pidx (PQUnitId unit_id) =
    lookupUnitId pidx unit_id

execPackageMain :: Verbosity
                -> Compiler
                -> PackageDBStack
                -> ProgramConfiguration
                -> PackageQualifier
                -> SymbolIdentifier
                -> [Way]
                -> [String]
                -> IO ()
execPackageMain verbosity comp pkg_db_stack prog_conf pkg_qual sym_id ways args
    | CompilerId GHC ver <- compilerId comp = do
        -- TODO: enumerating all packages is silly slow
        pidx <- getInstalledPackages verbosity comp pkg_db_stack prog_conf

        let Just ipi = lookupPackageQualifier pidx pkg_qual
            Just base_ipi = lookupPackageQualifier pidx (PQName $ PackageName "base")
            Just rts_ipi = lookupPackageQualifier pidx (PQName $ PackageName "rts")

            PackageIdentifier (PackageName pkg_name) _ = sourcePackageId ipi
            unit_id = installedUnitId ipi
            pkg_key = compatPackageKey ipi
            Symbol sym = mkSymbol pkg_key sym_id "closure"

            ipis = [ipi, base_ipi, rts_ipi]

            libdirs = libraryDirs `concatMap` ipis

            comp_abi = showGHCCompilerIdWithAbi comp

        libs <- forM ipis $ \ipi -> do
          let PackageIdentifier (PackageName pkg_name) _ = sourcePackageId ipi
              [lib] = filter (isPrefixOf ("HS"++pkg_name)) $ hsLibraries ipi

          let ways' = filter wayRTSOnly ways
              lib' | "rts" <- pkg_name, [] <- ways' = lib
                   | "rts" <- pkg_name =
                       lib ++ "_" ++ intercalate "-" (map wayTag ways')
                   | otherwise = lib

          Just lib'' <- findLibrary libdirs comp_abi lib'
          return lib''

        (ghc, _) <- requireProgram verbosity ghcProgram prog_conf
        Just libDir <- lookup "LibDir" <$> GHC.getGhcInfo verbosity ghc

        libexec <- getLibexecDir

        let loader_args = concat [[sym], libs, ["--"], args]

        executeFile (libexec </> "rts-loader") False loader_args Nothing

        return ()

showGHCCompilerId :: Compiler -> String
showGHCCompilerId comp = let
    CompilerId flav ver = compilerId comp
  in
    display flav ++ display ver

showGHCCompilerIdWithAbi :: Compiler -> String
showGHCCompilerIdWithAbi comp =
  showGHCCompilerId comp ++
  case compilerAbiTag comp of
    NoAbiTag  -> []
    AbiTag xs -> xs

findLibrary :: [FilePath] -> String -> String -> IO (Maybe FilePath)
findLibrary dirs comp_abi lib = do
    let (prefix, ext) = libraryPathStuff
        lib' = prefix ++ intercalate "-" [lib, comp_abi] <.> ext
    findFile dirs lib'

libraryPathStuff =
    case os of
      "linux"   -> ("lib", "so")

      -- untested
      "mingw32" -> ("", "dll")
      "cygwin"  -> ("", "dll")
      "darwin"  -> ("", "dylib")
      _ -> error "libraryExtension: unknown operating system"

type ModuleName = String
type OccName = String
data Symbol = Symbol String

-- See 'nameToCLabel' in compiler/ghci/ByteCodeLink.hs
mkSymbol :: String -> SymbolIdentifier -> String -> Symbol
mkSymbol pkg_key (SymbolIdentifier module_name occ_name) suffix =
    Symbol sym
 where
   sym = concat
     [ prefixUnderscore
     , if pkg_key == mainPackageKey then "" else zEncodeString pkg_key ++ "_"
     , zEncodeString module_name
     , '_':zEncodeString occ_name
     , '_':suffix
     ]

mainPackageKey :: String
mainPackageKey = "main"

prefixUnderscore :: String
prefixUnderscore =
  case (os,arch) of
    ("mingw32","x86_64") -> ""
    ("cygwin","x86_64") -> ""
    ("mingw32",_) -> "_"
    ("darwin",_) -> "_"
    ("cygwin",_) -> "_"
    _ -> ""
