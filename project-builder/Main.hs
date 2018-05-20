{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cabal.Plan
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import Data.Binary
import Data.Foldable
import Data.Hashable
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Text hiding (find, intercalate)
import qualified Data.Text as T
import Data.Typeable
import Development.Shake
import Development.Shake.FilePath

newtype AppBuildPath = AppBuildPath () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult AppBuildPath = FilePath

pureScriptModules :: [(FilePattern, String)]
pureScriptModules = [ ("main.js", "Main")
                    ]

serverPlanJsonToPath :: PlanJson -> IO FilePath
serverPlanJsonToPath planJson = do
  let arch = unpack $ pjArch planJson
  let os = unpack $ pjOs planJson
  let ghcVer = unpack $ dispPkgId $ pjCompilerId planJson
  projectUnit <- maybe (fail "Something is not right.") return $ find (\(Unit _ (PkgId (PkgName name) _) _ _ _ _ _) -> name == pack "ttt-server") $ pjUnits planJson
  let projectVersion = unpack $ dispPkgId $ uPId projectUnit
  return $ intercalate "/" ["build", arch ++ "-" ++ os, ghcVer, projectVersion, "x", "ttt-server", "build", "ttt-server", "ttt-server"]

determineBuildPath :: MonadIO m => AppBuildPath -> m FilePath
determineBuildPath (AppBuildPath _) = liftIO $ do
  planDetails <- findAndDecodePlanJson $ Just "_build/server"
  path <- serverPlanJsonToPath $ fst planDetails
  return path

buildPureScriptProject :: String
buildPureScriptProject = "psc-package build -- --output ../project-builder/_build/compiled-purescript"

buildServerProject :: String
buildServerProject = "cabal new-build --builddir=../project-builder/_build/server"

getStdout :: (Exit, Stdout String, Stderr String) -> String
getStdout (_, (Stdout contents), _) = contents

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build", shakeThreads = 0} $ do
  let serverExecutable = "_build/ttt-server" <.> exe
  let bundledPurescriptModules = fmap (\m -> "_build/modules" </> (fst m)) pureScriptModules

  getBuildPath <- addOracle determineBuildPath
  
  want ([serverExecutable] ++ bundledPurescriptModules)

  "clean" ~> do
    putNormal "Cleaning Project"
    removeFilesAfter "_build" ["//*"]

  "_build/.psc-package.json" %> \out -> do
    putNormal "Build Purescript Code"
    pureScriptCodeFiles <- getDirectoryFiles "../frontend/src" ["//*.purs"]
    need $ fmap ("../frontend/src" </>) pureScriptCodeFiles
    cmd_ (Cwd "../frontend") buildPureScriptProject
    copyFileChanged "../frontend/psc-package.json" out

  forM_ pureScriptModules $ \(builtFile, psModule) -> ("_build/modules/" ++ builtFile) %> \out -> do
    putNormal ("Create Purescript Module " ++ psModule)
    need ["_build/.psc-package.json"]
    compiledPureScriptFiles <- getDirectoryFiles "_build/compiled-purescript" ["//*.js"]
    need $ fmap ("_build/compiled-purescript" </>) compiledPureScriptFiles
    bundleResult <- cmd (Cwd "_build/compiled-purescript") "purs" "bundle" "**/*.js" "--module" psModule
    writeFileChanged out $ getStdout bundleResult

  serverExecutable %> \_ -> do
    putNormal "Build Server Executable"
    serverFiles <- getDirectoryFiles "../server/src" ["//*.hs"]
    need ["../server/ttt-server.cabal"]
    need $ fmap ("../server/src/" </>) serverFiles
    cmd_ (Cwd "../server") buildServerProject
    projectBuildPath <- getBuildPath $ AppBuildPath ()
    copyFileChanged ("_build/server/" ++ projectBuildPath) "_build/ttt-server"
