module Board where

import Types
import Rules (isInsideBoard) 

initialBoard :: Board
initialBoard = 
    [ [Nothing, Just (Regular Black), Nothing, Just (Regular Black), Nothing, Just (Regular Black), Nothing, Just (Regular Black)]
    , [Just (Regular Black), Nothing, Just (Regular Black), Nothing, Just (Regular Black), Nothing, Just (Regular Black), Nothing]
    , [Nothing, Just (Regular Black), Nothing, Just (Regular Black), Nothing, Just (Regular Black), Nothing, Just (Regular Black)]
    , replicate 8 Nothing
    , replicate 8 Nothing
    , [Just (Regular White), Nothing, Just (Regular White), Nothing, Just (Regular White), Nothing, Just (Regular White), Nothing]
    , [Nothing, Just (Regular White), Nothing, Just (Regular White), Nothing, Just (Regular White), Nothing, Just (Regular White)]
    , [Just (Regular White), Nothing, Just (Regular White), Nothing, Just (Regular White), Nothing, Just (Regular White), Nothing]
    ]

getPieceAt :: Board -> Position -> Maybe Piece
getPieceAt board (row, col)
    | isInsideBoard (row, col) = board !! row !! col
    | otherwise = Nothing

setPieceAt :: Board -> Position -> Maybe Piece -> Board
setPieceAt board (row, col) piece =
    take row board ++
    [take col (board !! row) ++ [piece] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board