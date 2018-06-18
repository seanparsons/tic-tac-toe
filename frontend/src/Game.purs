module Game where

import Control.Monad.Aff (launchAff, Fiber)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Data.Argonaut.Core
import Data.StrMap
import Network.HTTP.Affjax
import Prelude

playMove :: String -> Number -> Number -> String -> Eff ( ajax :: AJAX, console :: CONSOLE | () ) (Fiber ( ajax :: AJAX, console :: CONSOLE | () ) Unit)
playMove gameID row column noughtOrCross = launchAff $ do
  let gameMove = fromObject
                 $ insert "player" (fromString gameID)
                 $ insert "row" (fromNumber row)
                 $ insert "column" (fromNumber column)
                 $ empty
  response <- post ("/game/" <> gameID <> "/playmove") gameMove
  liftEff $ log $ "POST response: " <> response.response
