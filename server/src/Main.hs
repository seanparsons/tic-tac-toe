{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Reader.Wiring
import Clay hiding ((!), empty)
import Data.Either
import Data.Foldable
import Data.IORef
import Data.HashMap.Strict hiding ((!))
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.HTML.Blaze
import System.Environment
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SA
import Text.Read hiding (lift)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Prelude hiding (lookup)

import Types
import Game

type GamesMap = HashMap T.Text Game
type AppEnv = IORef GamesMap
type AppM = ReaderT AppEnv Handler

type Homepage = H.Html
type API = Get '[HTML] Homepage
      :<|> "game" :> Get '[HTML] H.Html
      :<|> "game" :> Capture "gameid" T.Text :> Get '[HTML] H.Html
      :<|> "game" :> Capture "gameid" T.Text :> "playmove" :> ReqBody '[JSON] GameMove :> Post '[JSON] (Maybe NoughtOrCross)
      :<|> "scripts" :> Raw

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = homepage
       :<|> newGamePage
       :<|> gamePage
       :<|> playMoveEndpoint
       :<|> (Tagged $ unTagged $ serveDirectoryWebApp "javascript")

siteCSS :: Css
siteCSS = do
  ".grid" ? do
    display $ flex
    flexDirection $ column
  ".grid-row" ? do
    display $ flex
    flexDirection $ row
  ".grid-square" ? do
    width $ px 250
    height $ px 250
    fontSize $ px 180
  ".square-content" ? do
    display $ inlineBlock
    top $ px 50
    left $ px 50
    marginTop $ px 0
    marginBottom $ px 0
    width $ px 150
    height $ px 150

getGames :: AppM GamesMap
getGames = do
  ref <- wiredAsk
  liftIO $ readIORef ref

modifyGamesMap :: (GamesMap -> GamesMap) -> AppM ()
modifyGamesMap transform = do
  ref <- wiredAsk
  liftIO $ atomicModifyIORef' ref (\m -> (transform m, ()))

renderNoughtOrCross :: Maybe NoughtOrCross -> H.Html
renderNoughtOrCross Nothing       = mempty
renderNoughtOrCross (Just Nought) = S.svg ! HA.class_ "square-content nought-square" $ do
  S.g $ do
    S.circle ! SA.cx "75" ! SA.cy "75" ! SA.r "65" ! SA.strokeWidth "10" ! SA.stroke "black" ! SA.fill "white"
renderNoughtOrCross (Just Cross)  = S.svg ! HA.class_ "square-content cross-square" $ do
  let rectangle = S.rect ! SA.x "0" ! SA.y "70" ! SA.width "150" ! SA.height "10" ! SA.fill "black"
  S.g ! SA.transform "rotate(45 75 75)" $ rectangle
  S.g ! SA.transform "rotate(135 75 75)" $ rectangle

renderPageContents :: H.Html -> H.Html
renderPageContents contents = H.docTypeHtml $ do
  H.head $ do
    H.title "Tic-Tac-Toe"
  H.body $ do
    H.h1 "Tic-Tac-Toe"
    contents
    H.style ! HA.type_ "text/css" $ do
      H.toMarkup $ render siteCSS

playMoveClickHandler :: T.Text -> Int -> Int -> Game -> H.AttributeValue
playMoveClickHandler gameID row column (Game _ noughtOrCross) =
  mconcat ["PS.Game.playMove('", fromString $ T.unpack gameID, "')(", fromString $ show row, ")(", fromString $ show column, ")('", fromString $ show noughtOrCross, "')()"]

renderGame :: T.Text -> Game -> H.Html
renderGame gameID game = renderPageContents $ do
  H.div ! HA.class_ "grid" $ do
    forM_ [1..gameSize] $ \row -> do
      H.div ! HA.class_ "grid-row" $ do
        forM_ [1..gameSize] $ \column -> do
          let elemName = H.stringValue ("square-" ++ (show row) ++ "-" ++ (show column))
          let handler = playMoveClickHandler gameID row column game
          H.div ! HA.id elemName ! HA.class_ "grid-square" ! HA.onclick handler $ do
            renderNoughtOrCross (game ^. cells . at (row, column))

homepage :: AppM Homepage
homepage = return $ renderPageContents $ mempty

gamePage :: T.Text -> AppM H.Html
gamePage gameID = do
  games <- getGames
  liftIO $ print $ show $ keys games
  let possibleGame = lookup gameID games
  case possibleGame of
    Nothing -> throwError err404
    Just g  -> return $ do
      renderGame gameID g
      H.script ! HA.src "/scripts/game.js" ! HA.type_ "application/javascript" $ do
        mempty
  
playMoveEndpoint :: T.Text -> GameMove -> AppM (Maybe NoughtOrCross)
playMoveEndpoint gameID (GameMove movePlayer moveRow moveColumn) = do
  liftIO $ print ("play move", gameID, movePlayer, moveRow, moveColumn)
  games <- getGames
  liftIO $ print "getGames"
  game <- maybe (throwError err404) return (lookup gameID games)
  liftIO $ print "throwError"
  let moveResult = playGameMove moveRow moveColumn movePlayer game
  liftIO $ print "getGames"
  liftIO $ print moveResult
  (updatedGame, possibleWinner) <- either (\_ -> throwError err412) return moveResult
  modifyGamesMap (insert gameID updatedGame)
  return possibleWinner

newGamePage :: AppM H.Html
newGamePage = do
  uuid <- liftIO $ do
    generated <- UUID.nextRandom
    return $ UUID.toText generated
  modifyGamesMap $ insert uuid newGame
  let redirectHeaders = [("Location", T.encodeUtf8 ("/game/" `mappend` uuid))]
  throwError $ err307 { errHeaders = redirectHeaders }
  
appNaturalTransform :: AppEnv -> AppM a -> Handler a
appNaturalTransform = flip runReaderT

app :: AppEnv -> Application
app env = serve api $ hoistServer api (appNaturalTransform env) server

main :: IO ()
main = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  gamesRef <- newIORef empty
  atomicModifyIORef' gamesRef (\m -> (set (at "cake") (Just newGame) m, ()))
  atomicModifyIORef' gamesRef (\m -> (set (at "cake" . _Just . cells . at (1, 1)) (Just Nought) m, ()))
  atomicModifyIORef' gamesRef (\m -> (set (at "cake" . _Just . cells . at (1, 2)) (Just Cross) m, ()))
  atomicModifyIORef' gamesRef (\m -> (set (at "cake" . _Just . cells . at (1, 3)) (Just Nought) m, ()))
  games <- readIORef gamesRef
  print $ show games
  Warp.run port $ app gamesRef
