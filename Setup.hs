{-# LANGUAGE ScopedTypeVariables #-}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Process
import System.Directory
import Control.Exception
import Distribution.Verbosity
import Distribution.System (buildPlatform, buildOS, OS(..))
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Program.Find
import System.FilePath
import Data.Maybe

-- The build process leverages a custom Setup configuration as described in Michivi's article (https://blog.michivi.com/posts/2022-08-cabal-setup/).
-- This approach allows us to dynamically inject the necessary --extra-lib-dirs and --extra-include-dirs flags during compilation, providing greater
-- flexibility for managing external dependencies.

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks =
        simpleUserHooks
            { preConf = \_ flags -> do
                rsMake (fromFlag $ configVerbosity flags)
                pure PD.emptyHookedBuildInfo

            , confHook = \a flags ->
                confHook simpleUserHooks a flags
                    >>= rsAddDirs

            , postClean = \_ flags _ _ ->
                rsClean (fromFlag $ cleanVerbosity flags)
            }

execCargo :: Verbosity -> String -> [String] -> IO ()
execCargo verbosity command args = do
    cargoPath <- findProgramOnSearchPath silent defaultProgramSearchPath "cargo"
    dir <- getCurrentDirectory
    let cargoExec = case cargoPath of
            Just (p, _) -> p
            Nothing -> "cargo"
        cargoArgs = command : args
        workingDir = Just dir
        thirdComponent (_, _, c) = c
    maybeExit . fmap thirdComponent $ rawSystemStdInOut verbosity cargoExec cargoArgs workingDir Nothing Nothing IODataModeBinary

rsMake :: Verbosity -> IO ()
rsMake verbosity = execCargo verbosity "build" ["--release", "--lib"]

rsAddDirs :: LocalBuildInfo -> IO LocalBuildInfo
rsAddDirs lbi' = do
    dir <- getCurrentDirectory
    let rustIncludeDir = dir
        rustLibDir = dir </> "target/release"
        updateLbi lbi = lbi{localPkgDescr = updatePkgDescr (localPkgDescr lbi)}
        updatePkgDescr pkgDescr = pkgDescr{PD.library = updateLib <$> PD.library pkgDescr}
        updateLib lib = lib{PD.libBuildInfo = updateLibBi (PD.libBuildInfo lib)}
        updateLibBi libBuild =
            libBuild
                { PD.includeDirs = rustIncludeDir : PD.includeDirs libBuild
                , PD.extraLibDirs = rustLibDir : PD.extraLibDirs libBuild
                }
    pure $ updateLbi lbi'

rsClean :: Verbosity -> IO ()
rsClean verbosity = execCargo verbosity "clean" []
