import Data.Maybe
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
  ( UserHooks (confHook, preConf),
     defaultMainWithHooks,
     simpleUserHooks,
  )
import Distribution.Simple.LocalBuildInfo
   ( LocalBuildInfo (localPkgDescr),
   )
import Distribution.Simple.Setup
   ( BuildFlags (buildVerbosity),
     ConfigFlags (configVerbosity),
     fromFlag,
   )
import Distribution.Simple.UserHooks
   ( UserHooks (buildHook, confHook),
   )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (localPkgDescr)
  , buildDir
  , compiler
  )
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory (getCurrentDirectory)
import Distribution.Simple.Utils
  ( notice
  , copyFileVerbose
  , createDirectoryIfMissingVerbose
  , rawSystemExit
  , info
  )
import Distribution.Simple.BuildPaths
  ( mkGenericSharedBundledLibName
  , mkGenericSharedLibName
  , mkGenericStaticLibName
  , dllExtension
  )

main :: IO ()
main =
   defaultMainWithHooks
     simpleUserHooks
       { confHook = rustConfHook
       , buildHook = rustBuildHook
       }

rustConfHook ::
   (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
   ConfigFlags ->
   IO LocalBuildInfo
rustConfHook (description, buildInfo) flags = do
   localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
   let packageDescription = localPkgDescr localBuildInfo
       library = fromJust $ PD.library packageDescription
       libraryBuildInfo = PD.libBuildInfo library
   dir <- getCurrentDirectory

   return localBuildInfo
     { localPkgDescr = packageDescription
       { PD.library = Just $ library
         { PD.libBuildInfo = libraryBuildInfo
           { PD.extraLibDirs = (dir ++ "/target/release") :
                               (dir ++ "/target/debug") :
             PD.extraLibDirs libraryBuildInfo
     } } } }

rustBuildHook ::
   PD.PackageDescription ->
   LocalBuildInfo ->
   UserHooks ->
   BuildFlags ->
   IO ()
rustBuildHook description localBuildInfo hooks flags = do
  putStrLn "******************************************************************"
  putStrLn "Call `cargo build --release` to build a dependency written in Rust"
  -- FIXME: add `--target $TARGET` flag to support cross-compiling to $TARGET
  rawSystemExit (fromFlag $ buildVerbosity flags) "cargo" ["build","--release"]
  putStrLn "... `rustc` compilation seems to succeed 🦀! Back to Cabal build:"
  putStrLn "******************************************************************"
  putStrLn "Back to Cabal build"

  dir <- getCurrentDirectory

  let
    verbosity = fromFlag $ buildVerbosity flags
    rustTargetDir = dir <> "/target"
    sourceBuildDir = rustTargetDir <> "/release"
    targetBuildDir = buildDir localBuildInfo
    staticSource = sourceBuildDir <> "/" <> mkGenericStaticLibName "sp1_verifier_hs"
    staticTarget = targetBuildDir <> "/" <> mkGenericStaticLibName "sp1_verifier_hs"

  copyFileVerbose verbosity staticSource staticTarget

  buildHook simpleUserHooks description localBuildInfo hooks flags
