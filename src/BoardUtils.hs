--------------------------- Autores de Trabalho ------------------------------------
--Nome: Luana Lauschner Avilez Vilarinho
--Matrícula: 202265062AC
--Nome: Geovanni Moreira Dias
--Matrícula: 202365171A
------------------------------------------------------------------------------------

module BoardUtils where

import Types

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
    | row >= 0 && row < 8 && col >= 0 && col < 8 = board !! row !! col
    | otherwise = Nothing

setPieceAt :: Board -> Position -> Maybe Piece -> Board
setPieceAt board (row, col) piece =
    take row board ++
    [take col (board !! row) ++ [piece] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board