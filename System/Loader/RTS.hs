-- |
-- Module      :  System.Loader.RTS
-- Copyright   :  Copyright (C) 2016  Daniel Gröber
-- License     :  GPLv3+
--
-- Stability   :  experimental
--
-- Portability : tested on Linux, might work on OSX, will support Win32 in the
--               future if possible
--
-- Dynamically load entrypoints from Haskell libraries independent of the RTS
-- version.
--

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module System.Loader.RTS
    (
      -- * Functions
      lookupLibraryInfo
    , resolveLibraryInfo
    , loaderInvocation
    , loaderExecutablePath

      -- * Type Synonyms
    , ModuleName
    , OccName

      -- * Datatypes
    , PackageQualifier(..)
    , SymbolIdentifier(..)

      -- * Internal Datatypes
    , LibraryInfo
    , ResolvedLibraryInfo

    , Way(..)
    , module Distribution.Simple.PackageIndex
    , module Distribution.Compiler
    ) where

import Control.Monad
import Data.Version
import Data.Maybe
import Data.List
import Text.Encoding.Z
import System.Info
import System.FilePath
import System.Directory

import GHC.Way

import Distribution.Text
import Distribution.Package hiding (installedUnitId)
import Distribution.Compiler
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo

import Paths_rts_loader

-- | Encodes the ways a package can be looked up in a 'PackageIndex'
data PackageQualifier = PQName PackageName -- ^ Identify a package by just its
                                           -- name, if multiple packages match
                                           -- the latest version is picked but
                                           -- which unit id is picked is
                                           -- undefined.
                      | PQId PackageId     -- ^ Identify a package by its name
                                           -- and version. If multiple packages
                                           -- are found with varying unit ids
                                           -- which one is picked is undefined.
                      | PQUnitId UnitId    -- ^ Fully qualified package only one
                                           -- package matching can be present in
                                           -- a package database.
                      deriving(Eq, Ord, Read, Show)

-- | Identify a binder (aka. a "symbol" at the object file level) within a
-- package.
data SymbolIdentifier = SymbolIdentifier ModuleName OccName
                      deriving(Eq, Ord, Read, Show)

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

-- | Information about a library extracted from a package database stack.
data LibraryInfo = LibraryInfo String [FilePath] [InstalledPackageInfo]

-- | Everything in 'LibraryInfo' plus fully resolved library paths.
data ResolvedLibraryInfo = ResolvedLibraryInfo LibraryInfo [FilePath]

        -- TODO: enumerating all packages is silly slow
--        pidx <- getInstalledPackages verbosity comp pkg_db_stack prog_conf


--    | CompilerId GHC ver <- compilerId comp = let
                  -- -> Compiler
                  --   -- ^ Compiler information used (among other things) to
                  --   -- construct the library suffix
                  --   -- ex. @libHSfoo-0.1-SOMEHASH-ghc7.10.3.so@
                  -- -> PackageDBStack
                  --   -- ^ Where to look for installed libraries
                  -- -> ProgramConfiguration
                  --   -- ^ We need @ghc-pkg@ to list installed libraries

-- | @lookupLibraryInfo pkg_index pkg_qual@
--
-- Gets all the information required for loading a library from a package
-- database out of the corresponding package index @pkg_index@. The desired
-- package is identified using @pkg_qual@ which allows doing the lookup by name,
-- name+version or the unique "unit id" associated with a package.
--
-- The @pkg_index@ must at the very least contain the packages "base", "rts" and
-- the package identified by @pkg_qual@. The packages "base" and "rts" are
-- required because of GHC bug
-- <https://ghc.haskell.org/trac/ghc/ticket/10352 #10352> thus this restriction might disappear eventually.
--
-- The required package index can be obtained using
-- @'Distribution.Simple.Configure.getInstalledPackages' verbosity comp
-- pkg_db_stack prog_conf@
lookupLibraryInfo :: InstalledPackageIndex -> PackageQualifier -> LibraryInfo
lookupLibraryInfo pidx pkg_qual = let
    lookupPkg = lookupPackageQualifier pidx

    Just ipi = lookupPackageQualifier pidx pkg_qual
    Just base_ipi = lookupPkg (PQName $ PackageName "base")
    Just rts_ipi = lookupPkg (PQName $ PackageName "rts")

    pkg_key = compatPackageKey ipi

    ipis = [ipi, base_ipi, rts_ipi]
    libdirs = libraryDirs `concatMap` ipis
  in
    LibraryInfo pkg_key libdirs ipis

-- | @resolveLibraryInfo comp ways li@
--
-- Find libraries mentioned in @li@ on the filesystem.
--
-- * @comp@ identifies the compiler (version, ABI etc.) and
--
-- * @ways@ allows looking for the different library versions, i.e. "plain",
-- "profiled", "threaded" and combinations thereof (note that order is
-- significant"). Usually you'll just want @[WayDyn]@ though even this is
-- implied so @[]@ would suffice for a plain library.
--
-- See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/CompilerWays GHC-Wiki/Commentary/Rts/CompilerWays>.
resolveLibraryInfo :: Compiler
                   -> [Way]
                   -> LibraryInfo
                   -> IO ResolvedLibraryInfo
resolveLibraryInfo comp ways li@(LibraryInfo _ libdirs ipis)
    | CompilerId GHC _ <- compilerId comp =
        ResolvedLibraryInfo li <$> forM ipis resolveLibrary
 where
   comp_abi = showGHCCompilerIdWithAbi comp
   resolveLibrary ipi = do
     let PackageIdentifier (PackageName pkg_name) _ = sourcePackageId ipi
         [lib] = filter (isPrefixOf ("HS"++pkg_name)) $ hsLibraries ipi

         ways' = filter wayRTSOnly ways
         lib' | "rts" <- pkg_name, [] <- ways' = lib
              | "rts" <- pkg_name =
                  lib ++ "_" ++ intercalate "-" (map wayTag ways')
              | otherwise = lib

     Just lib'' <- findLibrary libdirs comp_abi lib'
     return lib''

-- | @loaderInvocation sym args rli@
--
-- Construct the commandline for the @rts-loader@ program which
--
-- * loads the library described by @rli@,
--
-- * executes @sym@ as its "main" function (i.e. it should be of type @IO ()@)
-- and
--
-- * passes the command line arguments @args@ via the environment.
loaderInvocation :: SymbolIdentifier
                -> [String] -- ^ Command line arguments to pass to hs_main
                -> ResolvedLibraryInfo
                -> [String]
loaderInvocation sym args (ResolvedLibraryInfo (LibraryInfo pkey _ _) libs) =
    let Symbol sym' = mkSymbol pkey sym "closure"
        loader_args = concat [[sym'], libs, ["--"], args]
    in
        loader_args

-- | Path to the "@rts-loader@" executable distributed along with this
-- library, "@$libexec/rts-loader@". If set the environment variable
-- @rts_loader_libexecdir@ is used instead of Cabal's "@$libexec@" variable
-- which was set during installation time.
loaderExecutablePath :: IO FilePath
loaderExecutablePath = (</> "rts-loader") <$> getLibexecDir

-- | Just the compiler flavor and version parts of 'showGHCCompilerIdWithAbi'
showGHCCompilerId :: Compiler -> String
showGHCCompilerId comp = let
    CompilerId flav ver = compilerId comp
  in
    display flav ++ display ver

-- | Print the library ABI string for GHC, returns something like @ghc7.10.3@
-- depending on the version. This is the postfix appended to shared library
-- names eg. "libHSzenc-0.1.1-5nxwDFCcrv6wKRoUdekV7-ghc7.10.3.so".

-- I'm not sure how other compilers handle this, probably none of them even
-- support shared libraries so what does it matter anyways ;)
showGHCCompilerIdWithAbi :: Compiler -> String
showGHCCompilerIdWithAbi comp =
  showGHCCompilerId comp ++
  case compilerAbiTag comp of
    NoAbiTag  -> []
    AbiTag xs -> xs

-- | @findLibrary dirs comp_abi lib@.
--
-- Search for Haskell library @lib@ in @dirs@ while taking into account the
--compiler id @comp_abi@ (see 'showGHCCompilerIdWithAbi') as well as relevant
--platform conventions namely library prefix and extension. @lib@ must be given
--without a "lib" prefix and without an extension.
findLibrary :: [FilePath] -> String -> String -> IO (Maybe FilePath)
findLibrary dirs comp_abi lib = do
    let (prefix, ext) = libraryPathStuff
        lib' = prefix ++ intercalate "-" [lib, comp_abi] <.> ext
    findFile dirs lib'

-- | Library prefix and extension based on 'System.Info.os', eg. on Linux
-- @("lib", "so")@
libraryPathStuff :: (String, String)
libraryPathStuff =
    case os of
      "linux"   -> ("lib", "so")

      -- untested
      "mingw32" -> ("", "dll")
      "cygwin"  -> ("", "dll")
      "darwin"  -> ("", "dylib")
      _ -> error "libraryPathStuff: unknown operating system"

-- | A regular Haskell module name eg. @System.Loader.RTS@
type ModuleName = String

-- | An unqualified name of a binder eg. @main@
type OccName = String

-- | The name of a symbol as it occurs in a compiled Haskell object file. Must
-- be constructed by 'mkSymbol'. The precise format varies across GHC versions,
-- see 'nameToCLabel' in compiler/ghci/ByteCodeLink.hs.
data Symbol = Symbol String

-- | @mkSymbol pkg_key (SymbolIdentifier module_name occ_name) suffix@
--
-- Constructs a well formed 'Symbol' when
--
-- * @pkg_key@ is a valid package key as can be found in the @key@ field of a
-- package database @.conf@ file,
--
-- * @module_name@ is a valid Haskell module name (modid),
--
-- * @occ_name@ is a valid Haskell variable identifier (varid),
--
-- * @suffix@ is at least one of @["con_info", "closure"]@ (other valid options
-- can be found scattered across the GHC source code. The most interesting
-- option is "closure" as this is what the entry points to functions are called.
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

-- I'm not sure I even need this here since we only deal with external libraries
-- but it shouldn't hurt
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
