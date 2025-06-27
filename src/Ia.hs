module AI (chooseMove) where

import Types
import Moves (getAllValidMoves, isCaptureMove)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Calcula a distância do movimento diagonal
moveDistance :: Move -> Int
moveDistance (Move (fr, fc) (tr, tc)) = max (abs (tr - fr)) (abs (tc - fc))

chooseMove :: GameState -> Move
chooseMove state =
    let
        boardState = board state
        player = currentPlayer state
        allMoves = getAllValidMoves state player

        -- Se há capturas pendentes, só considere movimentos dessas posições
        filteredMoves = case pendingCaptures state of
            [] -> allMoves
            pcs -> filter (\(Move from _) -> from `elem` pcs) allMoves

        captures = filter (isCaptureMove boardState) filteredMoves
    in
        if not (null captures)
            then maximumBy (comparing moveDistance) captures
            else head filteredMoves
