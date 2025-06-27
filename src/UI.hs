module UI (
    gameLoop,
    playerVsBotLoop,
    botVsBotLoop
) where

import Types
import BoardUtils
import GameLogic (processMove, initialGameState)
import Moves (getAllValidMoves)
import AI (chooseMove)
import System.IO
import Control.Monad (when)
import Control.Concurrent (threadDelay)

-- Mostra o tabuleiro
displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn "   A B C D E F G H"         -- cabeçalho das colunas
    mapM_ (\(i, row) ->
        putStrLn (show (8 - i) ++ " " ++ showRow row)
      ) (zip [0..7] board)
  where
    showRow = unwords . map showCell
    showCell Nothing                  = "."
    showCell (Just (Regular White))   = "w"
    showCell (Just (Regular Black))   = "b"
    showCell (Just (King    White))   = "W"
    showCell (Just (King    Black))   = "B"

-- Lê movimento do jogador
getMove :: IO Move
getMove = do
    putStr "Digite movimento (ex: '2 3 3 4'): "
    hFlush stdout
    input <- getLine
    let ws = words input
    if length ws /= 4
        then do
            putStrLn "Entrada inválida! Use o formato: linha coluna linha coluna."
            getMove
        else do
            let [fromRow, fromCol, toRow, toCol] = map read ws
            return $ Move (fromRow, fromCol) (toRow, toCol)

-- Loop principal do jogo (Jogador vs Jogador)
gameLoop :: GameState -> IO ()
gameLoop state = do
    displayBoard (board state)
    if gameOver state
        then putStrLn $ "Fim de jogo! Vencedor: " ++ show (winner state)
        else do
            putStrLn $ "Vez de: " ++ show (currentPlayer state)
            move <- getMove
            let newState = processMove state move
            if board newState == board state
                then do
                    putStrLn "Movimento inválido! Tente novamente."
                    gameLoop state
                else gameLoop newState

-- Jogador (White) vs IA (Black)
playerVsBotLoop :: GameState -> IO ()
playerVsBotLoop state = do
    displayBoard (board state)
    if gameOver state
        then putStrLn $ "Fim de jogo! Vencedor: " ++ show (winner state)
        else do
            putStrLn $ "Vez de: " ++ show (currentPlayer state)
            if currentPlayer state == White
                then do
                    move <- getMove
                    let newState = processMove state move
                    if board newState == board state
                        then do
                            putStrLn "Movimento inválido! Tente novamente."
                            playerVsBotLoop state
                        else playerVsBotLoop newState
                else do
                    putStrLn "Vez da máquina..."
                    threadDelay 1000000
                    let move = chooseMove state
                    putStrLn $ "Máquina escolheu: " ++ show move
                    let newState = processMove state move
                    playerVsBotLoop newState

-- Bot vs Bot
botVsBotLoop :: GameState -> IO ()
botVsBotLoop state = do
    displayBoard (board state)
    if gameOver state
        then putStrLn $ "Fim de jogo! Vencedor: " ++ show (winner state)
        else do
            putStrLn $ "Vez do bot: " ++ show (currentPlayer state)
            threadDelay 1000000
            let move = chooseMove state
            putStrLn $ "Bot escolheu: " ++ show move
            let newState = processMove state move
            botVsBotLoop newState
