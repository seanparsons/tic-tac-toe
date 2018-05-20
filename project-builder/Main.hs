{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cabal.Plan
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import Data.Binary
import Data.Foldable
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Text hiding (find)
import qualified Data.Text as T
import Data.Typeable
import Development.Shake
import Development.Shake.FilePath

newtype AppBuildPath = AppBuildPath () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult AppBuildPath = FilePath

pureScriptModules :: [(FilePattern, String)]
pureScriptModules = [ ("homepage.js", "Homepage")
                    ]

serverPlanJsonToPath :: PlanJson -> IO FilePath
serverPlanJsonToPath planJson = do
  let arch = pjArch planJson
  let os = pjOs planJson
  let ghcVer = dispPkgId $ pjCompilerId planJson
  projectUnit <- maybe (fail "Something is not right.") return $ find (\(Unit _ (PkgId (PkgName name) _) _ _ _ _ _) -> name == "ttt-server") $ pjUnits planJson
  let projectVersion = dispPkgId $ uPId projectUnit
  return $ unpack $ T.intercalate "/" ["build", arch `mappend` "-" `mappend` os, ghcVer, projectVersion, "x", "ttt-server", "build", "ttt-server", "ttt-server"]

determineBuildPath :: MonadIO m => AppBuildPath -> m FilePath
determineBuildPath (AppBuildPath _) = liftIO $ do
  planDetails <- findAndDecodePlanJson $ Just "_build/server"
  path <- serverPlanJsonToPath $ fst planDetails
  return path

buildPureScriptProject :: String
buildPureScriptProject = "psc-package build -- --output ../project-builder/_build/compiled-purescript"

buildPureScriptBundle :: String
buildPureScriptBundle = "purs bundle \"_build/compiled-purescript/**/*.js\" --module"

buildServerProject :: String
buildServerProject = "cabal new-build --builddir=../project-builder/_build/server"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  let serverExecutable = "_build/ttt-server" <.> exe

  getBuildPath <- addOracle determineBuildPath
  
  want [serverExecutable]

  "clean" ~> do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  "_build/compiled-purescript//*.js" %> \_ -> do
    pureScriptFiles <- getDirectoryFiles "../frontend/src" ["//*.purs"]
    need pureScriptFiles
    cmd_ (Cwd "../frontend") buildPureScriptProject

  forM_ pureScriptModules $ \(builtFile, psModule) -> ("_build/modules/" ++ builtFile) %> \out -> do
    compiledPureScriptFiles <- getDirectoryFiles "_build/compiled-purescript" ["//*.js"]
    need compiledPureScriptFiles
    cmd_ buildPureScriptBundle psModule (">" :: String) out

  serverExecutable %> \_ -> do
    putNormal "Build server executable"
    serverFiles <- getDirectoryFiles "../server/src" ["//*.hs"]
    let frontEndFiles = fmap fst pureScriptModules
    let neededFiles = fmap ("../server/src" </>) serverFiles ++ fmap ("_build/modules" </>) frontEndFiles ++ ["../server/ttt-server.cabal"]
    putNormal $ show neededFiles
    need neededFiles
    cmd_ (Cwd "../server") buildServerProject
    projectBuildPath <- getBuildPath $ AppBuildPath ()
    copyFileChanged ("_build/server/" ++ projectBuildPath) "_build/ttt-server"
