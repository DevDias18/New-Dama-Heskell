module Moves (
    makeMove,
    getAllValidMoves,
    findCaptureMoves
) where

import Types
import BoardUtils
import Rules (opponent, pieceColor, isInsideBoard)
import Data.List (find)
import Data.Maybe (isJust, fromJust, isNothing)



-- Funções auxiliares
middlePosition :: Position -> Position -> Position
middlePosition (fromRow, fromCol) (toRow, toCol) =
    ((fromRow + toRow) `div` 2, (fromCol + toCol) `div` 2)

isDiagonalMove :: Position -> Position -> Bool
isDiagonalMove (r1, c1) (r2, c2) = abs (r1 - r2) == abs (c1 - c2)

-- Verifica se é um movimento de captura
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

-- Verifica movimento válido
isValidMove :: GameState -> Move -> Bool
isValidMove state move@(Move from to)
    | not (isInsideBoard from) || not (isInsideBoard to) = False
    | isJust (getPieceAt (board state) to) = False
    | not (null (pendingCaptures state)) = 
        from `elem` pendingCaptures state && isCaptureMove (board state) move
    | otherwise = 
        any (isValidNormalMove (board state) (currentPlayer state)) [move] ||
        isCaptureMove (board state) move

-- Verifica movimento normal (sem captura)
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

-- Encontra movimentos de captura
findCaptureMoves :: Board -> Position -> Piece -> [Move]
findCaptureMoves board pos piece =
    let directions = case piece of
            Regular White -> [NorthWest, NorthEast]
            Regular Black -> [SouthWest, SouthEast]
            King _ -> [NorthWest, NorthEast, SouthWest, SouthEast]
        possibleMoves = concatMap (getCaptureMovesInDirection board pos piece) directions
    in possibleMoves

-- Obtém capturas em uma direção
getCaptureMovesInDirection :: Board -> Position -> Piece -> Direction -> [Move]
getCaptureMovesInDirection board (row, col) piece dir =
    let (deltaRow, deltaCol) = case dir of
            NorthWest -> (-1, -1)
            NorthEast -> (-1, 1)
            SouthWest -> (1, -1)
            SouthEast -> (1, 1)
        nextPos = (row + deltaRow, col + deltaCol)
        jumpPos = (row + 2*deltaRow, col + 2*deltaCol)
    in case getPieceAt board nextPos of
        Just (Regular c) | c /= pieceColor piece ->
            if isInsideBoard jumpPos && isNothing (getPieceAt board jumpPos)
            then [Move (row, col) jumpPos]
            else []
        Just (King c) | c /= pieceColor piece ->
            if isInsideBoard jumpPos && isNothing (getPieceAt board jumpPos)
            then [Move (row, col) jumpPos]
            else []
        _ -> []

-- Verifica se pode capturar novamente
canCaptureAgain :: Board -> Position -> Bool
canCaptureAgain board pos =
    case getPieceAt board pos of
        Just piece -> not (null (findCaptureMoves board pos piece))
        Nothing -> False

-- Verifica promoção a dama
checkPromotion :: Board -> Position -> Board
checkPromotion board (row, col) =
    case getPieceAt board (row, col) of
        Just (Regular White) | row == 0 -> setPieceAt board (row, col) (Just (King White))
        Just (Regular Black) | row == 7 -> setPieceAt board (row, col) (Just (King Black))
        _ -> board

-- Executa um movimento
makeMove :: GameState -> Move -> GameState
makeMove state move@(Move from to) =
    if not (isValidMove state move)
        then state
        else 
            let piece = fromJust (getPieceAt (board state) from)
                newBoard = setPieceAt (setPieceAt (board state) from Nothing) to (Just piece)
                (finalBoard, capturedPos) = if isCaptureMove (board state) move
                                            then let (midRow, midCol) = middlePosition from to
                                                 in (setPieceAt newBoard (midRow, midCol) Nothing, Just (midRow, midCol))
                                            else (newBoard, Nothing)
                
                promotedBoard = checkPromotion finalBoard to
                (nextPlayer, newPending) = if isJust capturedPos && canCaptureAgain promotedBoard to
                                          then (currentPlayer state, [to])
                                          else (opponent (currentPlayer state), [])
                
                newState = state { board = promotedBoard
                                 , currentPlayer = nextPlayer
                                 , pendingCaptures = newPending
                                 }
            in newState

-- Obtém todos os movimentos válidos para um jogador
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

-- Obtém movimentos normais (sem captura) para uma peça
getNormalMoves :: Board -> Position -> Piece -> [Move]
getNormalMoves board (row, col) piece =
    let directions = case piece of
            Regular White -> [NorthWest, NorthEast]
            Regular Black -> [SouthWest, SouthEast]
            King _ -> [NorthWest, NorthEast, SouthWest, SouthEast]
        possibleMoves = map (getNormalMoveInDirection board (row, col)) directions
    in filter (\(Move _ to) -> isInsideBoard to && isNothing (getPieceAt board to)) possibleMoves

-- Obtém movimento normal em uma direção
getNormalMoveInDirection :: Board -> Position -> Direction -> Move
getNormalMoveInDirection board (row, col) dir =
    let (deltaRow, deltaCol) = case dir of
            NorthWest -> (-1, -1)
            NorthEast -> (-1, 1)
            SouthWest -> (1, -1)
            SouthEast -> (1, 1)
    in Move (row, col) (row + deltaRow, col + deltaCol)