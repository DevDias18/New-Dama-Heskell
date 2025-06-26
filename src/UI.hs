module UI where

import Types
import BoardUtils
import GameLogic (processMove, initialGameState)
import Moves (getAllValidMoves)
import System.IO
import Control.Monad (when)
-- Mostra o tabuleiro
displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn "  0 1 2 3 4 5 6 7"
    mapM_ (\(i, row) -> putStrLn (show i ++ " " ++ showRow row)) (zip [0..7] board)
    where
        showRow = unwords . map showCell
        showCell Nothing = "."
        showCell (Just (Regular White)) = "w"
        showCell (Just (Regular Black)) = "b"
        showCell (Just (King White)) = "W"
        showCell (Just (King Black)) = "B"

-- Lê movimento do jogador
getMove :: IO Move
getMove = do
    putStr "Digite movimento (ex: '2 3 3 4'): "
    hFlush stdout
    input <- getLine
    let [fromRow, fromCol, toRow, toCol] = map read (words input)
    return $ Move (fromRow, fromCol) (toRow, toCol)

-- Loop principal do jogo
gameLoop :: GameState -> IO ()
gameLoop state = do
    displayBoard (board state)
    if gameOver state
        then putStrLn $ "Fim de jogo! Vencedor: " ++ show (winner state)
        else do
            move <- getMove 
            let newState = processMove state move
            if board newState == board state
                then do
                    putStrLn "Movimento inválido! Tente novamente."
                    gameLoop state
                else gameLoop newState