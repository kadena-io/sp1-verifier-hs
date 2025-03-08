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
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory (getCurrentDirectory)

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
   buildHook simpleUserHooks description localBuildInfo hooks flags
