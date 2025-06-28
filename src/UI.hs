--------------------------- Autores de Trabalho ------------------------------------
--Nome: Luana Lauschner Avilez Vilarinho
--Matrícula: 202265062AC
--Nome: Geovanni Moreira Dias
--Matrícula: 202365171A
------------------------------------------------------------------------------------

module UI (
    gameLoop,
    playerVsBotLoop,
    botVsHumanLoop,
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
import Data.Char (toUpper, isDigit, ord)

-- Mostra o tabuleiro
displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn "  A B C D E F G H"           -- cabeçalho das colunas
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
parseCoord :: String -> Maybe Position
parseCoord s
  | length s < 2 = Nothing
  | otherwise =
      let (rowStr, [colCh]) = splitAt (length s - 1) s
      in  if all isDigit rowStr
             then let row = read rowStr
                      col = ord (toUpper colCh) - ord 'A'
                  in if row >= 1 && row <= 8 && col >= 0 && col <= 7
                        then Just (8 - row, col)   -- 8→0, 1→7
                        else Nothing
             else Nothing

getMove :: IO Move
getMove = do
    putStr "Digite movimento (ex: '3A 4B'): "
    hFlush stdout
    ws <- words <$> getLine
    case ws of
        [srcStr, dstStr]
            | Just src <- parseCoord srcStr
            , Just dst <- parseCoord dstStr  -> return (Move src dst)
        _ -> do
            putStrLn "Entrada inválida! Use o formato linhaColuna linhaColuna (ex: 3A 4B)."
            getMove


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

-- Bot joga com as peças brancas; humano joga com as pretas
botVsHumanLoop :: GameState -> IO ()
botVsHumanLoop state = do
    displayBoard (board state)
    if gameOver state
        then putStrLn $ "Fim de jogo! Vencedor: " ++ show (winner state)
        else do
            putStrLn $ "Vez de: " ++ show (currentPlayer state)
            if currentPlayer state == White
                then do                               -- turno do Bot
                    putStrLn "Vez da máquina..."
                    threadDelay 1000000
                    let move = chooseMove state
                    putStrLn $ "Máquina escolheu: " ++ show move
                    let newState = processMove state move
                    botVsHumanLoop newState
                else do                               -- turno do Humano (peças pretas)
                    move <- getMove
                    let newState = processMove state move
                    if board newState == board state
                       then do
                           putStrLn "Movimento inválido! Tente novamente."
                           botVsHumanLoop state
                       else botVsHumanLoop newState
