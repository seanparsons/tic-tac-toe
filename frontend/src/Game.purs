module Game where

import Control.Monad.Aff (runAff, Fiber)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut.Core
import Data.Either (Either(Left, Right))
import Data.StrMap (empty, insert)
import Network.HTTP.Affjax (AJAX, post)
import Prelude (Unit, bind, pure, unit, ($), (<>))

handleError :: forall a e. Either Error a -> Eff (console :: CONSOLE | e ) Unit
handleError (Left error) = errorShow error
handleError (Right _)    = pure unit

playMove :: String -> Number -> Number -> String -> Eff ( ajax :: AJAX, console :: CONSOLE | () ) (Fiber ( ajax :: AJAX, console :: CONSOLE | () ) Unit)
playMove gameID row column noughtOrCross = runAff handleError $ do
  let gameMove = fromObject
                 $ insert "player" (fromString noughtOrCross)
                 $ insert "row" (fromNumber row)
                 $ insert "column" (fromNumber column)
                 $ empty
  response <- post ("/game/" <> gameID <> "/playmove") gameMove
  liftEff $ log $ "POST response: " <> response.response
