{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Clay hiding ((!))
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

siteCSS :: Css
siteCSS = Clay.header |> nav ? do
  td # "#grid-square" ? do
    width $ px 300
    height $ px 300

homepage :: Homepage
homepage = H.docTypeHtml $ do
  H.head $ do
    H.title "Tic-Tac-Toe"
  H.body $ do
    H.h1 "Tic-Tac-Toe"
    H.table $ do
      forM_ [0..2] $ \row -> do
        H.tr ! HA.class_ "grid-row" $ do
          forM_ [0..2] $ \column -> do
            H.td ! HA.class_ "grid-square" $ H.toMarkup $ show (row, column)
    H.script $ H.toMarkup $ render siteCSS


app :: Application
app = serve api server

main :: IO ()
main = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  Warp.run port app
