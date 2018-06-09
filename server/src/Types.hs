{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Hashable
import Data.HashMap.Strict
import GHC.Generics

data NoughtOrCross = Nought
                   | Cross
                   deriving (Eq, Show, Ord, Generic)

instance Hashable NoughtOrCross

data GameMoveError = PositionOccupied
                   | NotYourTurn
                   deriving (Eq, Show, Ord, Generic)

data Game = Game
          { _cells       :: HashMap (Int, Int) NoughtOrCross
          , _toPlayNext  :: NoughtOrCross
          } deriving (Eq, Show, Ord, Generic)

newGame :: Game
newGame = Game empty Cross

