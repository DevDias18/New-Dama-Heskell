module Main where

import UI
import GameLogic (initialGameState)
import Types
import Data.Char (toLower)
import Control.Monad (when)

main :: IO ()
main = mainLoop

mainLoop :: IO ()
mainLoop = do
    putStrLn "Modo de jogo:"
    putStrLn "1 - Jogador vs Jogador"
    putStrLn "2 - Jogador vs Computador"
    putStrLn "3 - Computador vs Computador"
    putStr "Escolha o modo: "
    mode <- getLine

    putStr "Quem começa? (White ou Black): "
    starter <- getLine
    let s = map toLower starter
    let color = if s == "white" then White else Black
    let initial = initialGameState { currentPlayer = color }

    case mode of
        "1" -> gameLoop initial
        "2" -> playerVsBotLoop initial
        "3" -> botVsBotLoop initial
        _   -> putStrLn "Modo inválido"

    putStrLn "Deseja jogar novamente? (s/n)"
    resp <- getLine
    when (map toLower resp == "s") mainLoop
