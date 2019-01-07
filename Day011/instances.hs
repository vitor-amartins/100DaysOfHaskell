data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ChessSquare = King | Queen | Rook | Bishop | Knight | Pawn | Empty
                   deriving (Show)
type ChessPosition = (Int, Char, ChessSquare)
type Chessboard = [ChessPosition]

