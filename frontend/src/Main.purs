module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

main :: Eff (console :: CONSOLE) Unit
main = do
  logShow "Hello World!"
