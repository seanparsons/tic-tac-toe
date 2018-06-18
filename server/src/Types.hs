{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Hashable
import Data.HashMap.Strict
import Data.Text hiding (empty, drop)
import GHC.Generics

data NoughtOrCross = Nought
                   | Cross
                   deriving (Eq, Ord, Generic)

instance Show NoughtOrCross where
  show Nought = "o"
  show Cross  = "x"

instance Hashable NoughtOrCross

instance FromJSON NoughtOrCross where
  parseJSON (String "x") = return Cross
  parseJSON (String "o") = return Nought
  parseJSON invalid    = typeMismatch "NoughtOrCross" invalid

instance ToJSON NoughtOrCross where
  toJSON Cross  = toJSON ("x" :: Text)
  toJSON Nought = toJSON ("o" :: Text)

data GameMoveError = PositionOccupied
                   | NotYourTurn
                   deriving (Eq, Show, Ord, Generic)

data Game = Game
          { _cells       :: HashMap (Int, Int) NoughtOrCross
          , _toPlayNext  :: NoughtOrCross
          } deriving (Eq, Show, Ord, Generic)

newGame :: Game
newGame = Game empty Cross

data GameMove = GameMove
              { _movePlayer   :: NoughtOrCross
              , _moveRow      :: Int
              , _moveColumn   :: Int
              } deriving (Eq, Show, Ord, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''GameMove)
