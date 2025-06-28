--------------------------- Autores de Trabalho ------------------------------------
--Nome: Luana Lauschner Avilez Vilarinho
--Matrícula: 202265062AC
--Nome: Geovanni Moreira Dias
--Matrícula: 202365171A
------------------------------------------------------------------------------------

module GameLogic where

import Types
import BoardUtils
import Rules (countPieces, opponent)
import Moves (makeMove, getAllValidMoves)


initialGameState :: GameState
initialGameState = GameState
    { board = initialBoard
    , currentPlayer = White
    , gameOver = False
    , winner = Nothing
    , pendingCaptures = []
    }

switchPlayer :: GameState -> GameState
switchPlayer state = state { currentPlayer = opponent (currentPlayer state) }
    where opponent White = Black
          opponent Black = White

-- Função para processar um movimento completo
processMove :: GameState -> Move -> GameState
processMove state move =
    let newState = makeMove state move
    in checkGameOver newState  

checkGameOver :: GameState -> GameState
checkGameOver state =
    let blackPieces = countPieces (board state) Black
        whitePieces = countPieces (board state) White
        currentColor = currentPlayer state
        hasValidMoves = not (null (getAllValidMoves state currentColor))
    in if blackPieces == 0
        then state { gameOver = True, winner = Just White }
        else if whitePieces == 0
            then state { gameOver = True, winner = Just Black }
            else if not hasValidMoves
                then state { gameOver = True, winner = Just (opponent currentColor) }
                else state