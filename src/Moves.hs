--------------------------- Autores de Trabalho ------------------------------------
--Nome: Luana Lauschner Avilez Vilarinho
--Matrícula: 202265062AC
--Nome: Geovanni Moreira Dias
--Matrícula: 202365171A
------------------------------------------------------------------------------------

module Moves (
    makeMove,
    getAllValidMoves,
    findCaptureMoves,
    isCaptureMove
) where

import Types
import BoardUtils
import Rules (opponent, pieceColor, isInsideBoard)
import Data.List (find)
import Data.Maybe (isJust, fromJust, isNothing)

middlePosition :: Position -> Position -> Position
middlePosition (fromRow, fromCol) (toRow, toCol) =
    ((fromRow + toRow) `div` 2, (fromCol + toCol) `div` 2)

isDiagonalMove :: Position -> Position -> Bool
isDiagonalMove (r1, c1) (r2, c2) = abs (r1 - r2) == abs (c1 - c2)

isCaptureMove :: Board -> Move -> Bool
isCaptureMove board (Move from to) =
    case getPieceAt board from of
        Nothing -> False
        Just piece ->
            let (middleRow, middleCol) = middlePosition from to
                middlePiece = getPieceAt board (middleRow, middleCol)
                isOpponent = case middlePiece of
                    Just (Regular c) -> c /= pieceColor piece
                    Just (King c) -> c /= pieceColor piece
                    Nothing -> False
            in isDiagonalMove from to &&
               isOpponent &&
               case piece of
                   Regular _ -> abs (fst to - fst from) == 2
                   King _ -> True

isValidMove :: GameState -> Move -> Bool
isValidMove state move@(Move from to)
    | not (isInsideBoard from) || not (isInsideBoard to) = False
    | isJust (getPieceAt (board state) to) = False
    | not (null (pendingCaptures state)) =
        from `elem` pendingCaptures state && isCaptureMove (board state) move
    | otherwise =
        any (isValidNormalMove (board state) (currentPlayer state)) [move] ||
        isCaptureMove (board state) move

isValidNormalMove :: Board -> PieceColor -> Move -> Bool
isValidNormalMove board color (Move (fromRow, fromCol) (toRow, toCol)) =
    case getPieceAt board (fromRow, fromCol) of
        Just (Regular pc) ->
            pc == color &&
            abs (fromCol - toCol) == 1 &&
            if pc == White
                then fromRow - toRow == 1
                else toRow - fromRow == 1
        Just (King _) ->
            abs (fromRow - toRow) == 1 &&
            abs (fromCol - toCol) == 1
        Nothing -> False

findCaptureMoves :: Board -> Position -> Piece -> [Move]
findCaptureMoves board pos piece =
    let directions = case piece of
            Regular White -> [NorthWest, NorthEast]
            Regular Black -> [SouthWest, SouthEast]
            King _ -> [NorthWest, NorthEast, SouthWest, SouthEast]
    in concatMap (getCaptureMovesInDirection board pos piece) directions

getCaptureMovesInDirection :: Board -> Position -> Piece -> Direction -> [Move]
getCaptureMovesInDirection board (row, col) piece dir =
    let (deltaRow, deltaCol) = case dir of
            NorthWest -> (-1, -1)
            NorthEast -> (-1, 1)
            SouthWest -> (1, -1)
            SouthEast -> (1, 1)
        nextPositions = tail $ iterate (\(r,c) -> (r + deltaRow, c + deltaCol)) (row, col)
        
        findCaptures [] = []
        findCaptures (pos:rest)
            | not (isInsideBoard pos) = []
            | otherwise =
                case getPieceAt board pos of
                    Nothing -> findCaptures rest
                    Just p -> if pieceColor p /= pieceColor piece
                              then let landingSquares = takeWhile (\p2 -> isInsideBoard p2 && isNothing (getPieceAt board p2)) (tail rest)
                                   in map (\landing -> Move (row, col) landing) landingSquares
                              else []
    in case piece of
        King _ -> findCaptures nextPositions
        _      ->
            let nextPos = (row + deltaRow, col + deltaCol)
                jumpPos = (row + 2*deltaRow, col + 2*deltaCol)
            in case getPieceAt board nextPos of
                Just p | pieceColor p /= pieceColor piece ->
                    if isInsideBoard jumpPos && isNothing (getPieceAt board jumpPos)
                    then [Move (row, col) jumpPos]
                    else []
                _ -> []

canCaptureAgain :: Board -> Position -> Bool
canCaptureAgain board pos =
    case getPieceAt board pos of
        Just piece -> not (null (findCaptureMoves board pos piece))
        Nothing -> False

checkPromotion :: Board -> Position -> Board
checkPromotion board (row, col) =
    case getPieceAt board (row, col) of
        Just (Regular White) | row == 0 -> setPieceAt board (row, col) (Just (King White))
        Just (Regular Black) | row == 7 -> setPieceAt board (row, col) (Just (King Black))
        _ -> board

makeMove :: GameState -> Move -> GameState
makeMove state move@(Move from to)
    | not (isValidMove state move) = state
    | otherwise =
        let piece = fromJust (getPieceAt (board state) from)
            boardAfterMove = setPieceAt (setPieceAt (board state) from Nothing) to (Just piece)
            boardAfterCapture = if isCaptureMove (board state) move
                                then let (midRow, midCol) = middlePosition from to
                                     in setPieceAt boardAfterMove (midRow, midCol) Nothing
                                else boardAfterMove
            promotedBoard = checkPromotion boardAfterCapture to
            promotedPiece = fromJust (getPieceAt promotedBoard to)
            (nextPlayer, newPending) = if isCaptureMove (board state) move && canCaptureAgain promotedBoard to
                                       then (currentPlayer state, [to])
                                       else (opponent (currentPlayer state), [])
        in state { board = promotedBoard
                 , currentPlayer = nextPlayer
                 , pendingCaptures = newPending
                 }

getAllValidMoves :: GameState -> PieceColor -> [Move]
getAllValidMoves state color =
    let allPositions = [(r,c) | r <- [0..7], c <- [0..7]]
        playerPieces = filter (\(r,c) ->
            case getPieceAt (board state) (r,c) of
                Just (Regular c) -> c == color
                Just (King c) -> c == color
                Nothing -> False
            ) allPositions
    in concatMap (\pos ->
        case getPieceAt (board state) pos of
            Just piece ->
                let normalMoves = getNormalMoves (board state) pos piece
                    captureMoves = findCaptureMoves (board state) pos piece
                in if not (null captureMoves) then captureMoves else normalMoves
            Nothing -> []
        ) playerPieces

getNormalMoves :: Board -> Position -> Piece -> [Move]
getNormalMoves board (row, col) piece =
    let directions = case piece of
            Regular White -> [NorthWest, NorthEast]
            Regular Black -> [SouthWest, SouthEast]
            King _ -> [NorthWest, NorthEast, SouthWest, SouthEast]
        possibleMoves = map (getNormalMoveInDirection board (row, col)) directions
    in filter (\(Move _ to) -> isInsideBoard to && isNothing (getPieceAt board to)) possibleMoves

getNormalMoveInDirection :: Board -> Position -> Direction -> Move
getNormalMoveInDirection board (row, col) dir =
    let (deltaRow, deltaCol) = case dir of
            NorthWest -> (-1, -1)
            NorthEast -> (-1, 1)
            SouthWest -> (1, -1)
            SouthEast -> (1, 1)
    in Move (row, col) (row + deltaRow, col + deltaCol)
