module Main where

import UI
import GameLogic (initialGameState)

main :: IO ()
main = gameLoop initialGameState