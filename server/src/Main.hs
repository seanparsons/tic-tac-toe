{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Data.Foldable
import Data.Maybe
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.HTML.Blaze
import System.Environment
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Read

type API = Get '[HTML] Homepage
type Homepage = H.Html

api :: Proxy API
api = Proxy

server :: Server API
server = return homepage

homepage :: Homepage
homepage = H.docTypeHtml $ do
  H.head $ do
    H.title "Tic-Tac-Toe"
  H.body $ do
    H.h1 "Tic-Tac-Toe"
    forM_ [0..2] $ \row -> do
      H.div ! HA.class_ "grid-row" $ do
        forM_ [0..2] $ \column -> do
          H.div ! HA.class_ "grid-square" $ H.toMarkup $ show (row, column)


app :: Application
app = serve api server

main :: IO ()
main = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  Warp.run port app
