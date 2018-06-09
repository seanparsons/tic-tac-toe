{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad
import Data.Either.Extra
import Data.HashMap.Strict
import Data.Monoid
import GHC.Generics
import Types

makeLenses ''Game

gameSize :: Int
gameSize = 3

gameRange :: [Int]
gameRange = [1..gameSize]

countNoughtsOrCrosses :: HashMap NoughtOrCross Int -> Maybe NoughtOrCross -> HashMap NoughtOrCross Int
countNoughtsOrCrosses map Nothing              = map
countNoughtsOrCrosses map (Just noughtOrCross) = insertWith (+) noughtOrCross 1 map

getWinner :: [(NoughtOrCross, Int)] -> Maybe NoughtOrCross
getWinner [(noughtOrCross, countOf)] = if countOf == gameSize then Just noughtOrCross else Nothing
getWinner _                          = Nothing

playGameMove :: Int -> Int -> NoughtOrCross -> Game -> Either GameMoveError (Game, Maybe NoughtOrCross)
playGameMove row column noughtOrCross game = do
  let key = (row, column)
  let playedAlready = game ^. cells . at key
  _ <- maybeToEither PositionOccupied playedAlready
  _ <- unless (game ^. toPlayNext == noughtOrCross) (Left NotYourTurn)
  let updatedGame = game & (cells . at key . _Just) .~ noughtOrCross
  Right (updatedGame, determineWinner updatedGame)

determineWinner :: Game -> Maybe NoughtOrCross
determineWinner game =
  let getPosition r c = game ^. cells . at (r, c)
      rowWinners = fmap (\r -> fmap (\c -> getPosition r c) gameRange) gameRange
      columnWinners = fmap (\c -> fmap (\r -> getPosition r c) gameRange) gameRange
      fromTopLeft = fmap (\rc -> getPosition rc rc) gameRange
      fromTopRight = fmap (\rc -> getPosition rc (gameSize - rc + 1)) gameRange
      allPossibleWinners = rowWinners ++ columnWinners ++ [fromTopLeft, fromTopRight]
      counts = fmap (foldl countNoughtsOrCrosses empty) allPossibleWinners
      winners = fmap getWinner $ fmap toList counts
  in  getFirst $ foldMap First winners
    
  
