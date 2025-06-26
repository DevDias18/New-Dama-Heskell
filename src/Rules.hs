module Rules where

import Types
import BoardUtils

opponent :: PieceColor -> PieceColor
opponent White = Black
opponent Black = White

pieceColor :: Piece -> PieceColor
pieceColor (Regular color) = color
pieceColor (King color) = color

countPieces :: Board -> PieceColor -> Int
countPieces board color =
    sum [1 | row <- board, cell <- row, 
             case cell of
                 Just (Regular c) -> c == color
                 Just (King c) -> c == color
                 Nothing -> False]

isInsideBoard :: Position -> Bool
isInsideBoard (row, col) = row >= 0 && row < 8 && col >= 0 && col < 8