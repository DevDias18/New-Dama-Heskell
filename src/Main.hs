--------------------------- Autores de Trabalho ------------------------------------
--Nome: Luana Lauschner Avilez Vilarinho
--Matrícula: 202265062AC
--Nome: Geovanni Moreira Dias
--Matrícula: 202365171A
------------------------------------------------------------------------------------

module Main where

import UI
import GameLogic (initialGameState)
import Types
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Control.Monad (when)

main :: IO ()
main = mainLoop

mainLoop :: IO ()
mainLoop = do
    putStrLn "Quem joga com as peças White?  (1) Humano  (2) Robô : "
    hFlush stdout
    whiteChoice <- getLine

    putStrLn "Quem joga com as peças Black?  (1) Humano  (2) Robô : "
    hFlush stdout
    blackChoice <- getLine

    let initial = initialGameState  -- Peças brancas sempre começam

    case (whiteChoice, blackChoice) of
        ("1", "1") -> gameLoop initial                              -- Humano vs Humano
        ("1", "2") -> playerVsBotLoop initial                       -- Humano (White) vs Bot (Black)
        ("2","1") -> botVsHumanLoop initial                         -- Bot (White) vs Humano (Black)
        ("2", "2") -> botVsBotLoop initial                          -- Bot vs Bot
        _ -> putStrLn "Opção inválida. Reinicie e escolha 1 ou 2."

    putStrLn "Deseja jogar novamente? (s/n)"
    resp <- getLine
    when (map toLower resp == "s") mainLoop