{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Reader.Wiring
import Clay hiding ((!), empty)
import Data.Foldable
import Data.IORef
import Data.HashMap.Strict hiding ((!))
import Data.Maybe
import qualified Data.Text as T
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.HTML.Blaze
import System.Environment
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Read hiding (lift)
import Prelude hiding (lookup)

import Types
import Game

type GamesMap = HashMap T.Text Game
type AppEnv = IORef GamesMap
type AppM = ReaderT AppEnv Handler

type Homepage = H.Html
type API = Get '[HTML] Homepage
      :<|> "game" :> Capture "gameid" T.Text :> Get '[HTML] H.Html

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = homepage :<|> gamePage

siteCSS :: Css
siteCSS = do
  ".grid-square" ? do
    width $ px 200
    height $ px 200

getGames :: AppM GamesMap
getGames = do
  ref <- wiredAsk
  liftIO $ readIORef ref

renderGame :: Maybe Game -> H.Html
renderGame possibleGame = do
       H.table $ do
       forM_ [1..gameSize] $ \row -> do
         H.tr ! HA.class_ "grid-row" $ do
           forM_ [1..gameSize] $ \column -> do
             let elemName = H.stringValue ("square-" ++ (show row) ++ "-" ++ (show column))
             H.td ! HA.id elemName ! HA.class_ "grid-square" $ do
               H.toMarkup $ show (row, column)


homepage :: AppM Homepage
homepage = return $ H.docTypeHtml $ do
  H.head $ do
    H.title "Tic-Tac-Toe"
  H.body $ do
    H.h1 "Tic-Tac-Toe"
    H.style ! HA.type_ "text/css" $ do
      H.toMarkup $ render siteCSS

gamePage :: T.Text -> AppM H.Html
gamePage gameID = do
  games <- getGames
  let possibleGame = lookup gameID games
  

appNaturalTransform :: AppEnv -> AppM a -> Handler a
appNaturalTransform e a = runReaderT a e

app :: AppEnv -> Application
app env = serve api $ hoistServer api (appNaturalTransform env) server

main :: IO ()
main = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  gamesRef <- newIORef empty
  Warp.run port $ app gamesRef
