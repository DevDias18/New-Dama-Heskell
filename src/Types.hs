--------------------------- Autores de Trabalho ------------------------------------
--Nome: Luana Lauschner Avilez Vilarinho
--Matrícula: 202265062AC
--Nome: Geovanni Moreira Dias
--Matrícula: 202365171A
------------------------------------------------------------------------------------

module Types where

data Piece = Regular PieceColor | King PieceColor
    deriving (Eq, Show)

data PieceColor = Black | White
    deriving (Eq, Show, Read)

type Position = (Int, Int)
type Board = [[Maybe Piece]]

data GameState = GameState
    { board :: Board
    , currentPlayer :: PieceColor
    , gameOver :: Bool
    , winner :: Maybe PieceColor
    , pendingCaptures :: [Position]
    } deriving (Show)

data Move = Move Position Position
    deriving (Eq, Show)

data Direction = NorthEast | NorthWest | SouthEast | SouthWest
    deriving (Eq, Show)