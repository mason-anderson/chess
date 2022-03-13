module Game where

import Data.Array
import Data.List
import Data.Bifunctor
import Data.Maybe

data PColor = White | Black
    deriving (Eq, Show)
data Piece = Pawn | Bishop | Knight | Rook | King | Queen
    deriving (Eq, Show)
data Square = SPiece Piece PColor | SEmpty
    deriving (Eq, Show)

type Board = (Array (Int,Int) Square)
type Pos = (Int,Int)

getColor :: Square -> Maybe PColor
getColor SEmpty = Nothing
getColor (SPiece _ c) = Just c

oppositeColor :: PColor -> PColor
oppositeColor White = Black
oppositeColor Black = White

onBoard :: Pos -> Bool
onBoard (x,y) = x >= 0 && x < 8 && y >= 0 && y < 8

-- >>> initialBoard ! (3,0)
-- SPiece Queen White

initialBoard :: Board
initialBoard = listArray ((0,0),(7,7)) $ concat $ transpose
    [ map ($ White) pieces
    , map ($ White) pawns
    , empty
    , empty
    , empty
    , empty
    , map ($ Black) pawns
    , map ($ Black) pieces
    ]
    where
        empty = SEmpty <$ [1..8]
        pawns = SPiece Pawn <$ [1..8]
        pieces = [SPiece Rook, SPiece Knight, SPiece Bishop, SPiece Queen, SPiece King, SPiece Bishop, SPiece Knight, SPiece Rook]

checkMate :: Board -> Maybe PColor
checkMate b = undefined

makeMove :: Pos -> Pos -> Board -> Board
makeMove iPos fPos board = board // [(iPos, SEmpty), (fPos, board ! iPos)]

legalMove :: Pos -> Pos -> (Pos,Pos) -> Board -> Bool
legalMove iPos fPos lastMove board = movePossible iPos fPos board && not (inCheck color (makeMove iPos fPos board))
    where
        color = fromJust $ getColor (board ! iPos)

-- return True if that color's king is in check
-- we do this by checking to see if the king's pos is a legal move for any pieces
inCheck :: PColor -> Board -> Bool
inCheck color board = any (\p -> movePossible p kingPos board) oppositeColorPiecesPos
    where
        kingPos = fst $ head $ filterKing $ assocs board
        filterKing = filter (\case (_,SPiece King c) -> c == color; _ -> False)

        oppositeColorPiecesPos = map fst $ filterOppColor $ assocs board
        filterOppColor = filter (\case (_, SPiece _ c) -> c /= color; _ -> False)


-- checks if a move is possible (not including moving in check)
movePossible :: Pos -> Pos -> Board -> Bool
movePossible i f b = f `elem` legalSquares
    where
        piece = b ! i
        -- return a list of legal squares for the piece at i
        legalSquares :: [Pos]
        legalSquares = case piece of
            (SPiece Pawn c) -> filter (legalTake c) [ bimap (+1) (dir 1) i, bimap (subtract 1) (dir 1) i] -- take on diagonals
                            ++ moveOne -- move 1
                            ++ [x | x<-[second (dir 2) i], not (null moveOne) && b ! x == SEmpty && (snd i == 1 || snd i == 6)] -- move 2 (first move only)
                -- dir returns the direction of movement for the correct color
                where dir x = if c == White then (+x) else subtract x
                      moveOne = [x | x<-[second (dir 1) i], onBoard x && b ! x == SEmpty]
                      legalTake c x = onBoard x && (case b ! x of
                                SEmpty -> False
                                (SPiece _ c2) -> c2 /= c
                          )

            (SPiece Bishop c) -> concat [path c i x y | x <- [-1, 1], y <- [-1, 1]]
            (SPiece Knight c) -> filter (legal c) [(fst i +x,snd i + y) | x<-[1,-1,2,-2], y<-[1,-1,2,-2], abs x  /= abs y]
            (SPiece Rook c) -> concat [bPath 1 0, bPath (-1) 0, bPath 0 1, bPath 0 (-1)]
                where bPath = path c i
            (SPiece King c) -> filter (legal c) [(fst i + x,snd i + y) | x <-[-1,0,1], y <- [-1,0,1], x /= 0 || y /= 0]
            (SPiece Queen c) -> concat [path c i x y | x <- [-1,0,1], y <- [-1,0,1], x /= 0 || y /= 0]
            _ -> []

        legal c x = onBoard x && ( case b ! x of
                SEmpty -> True
                (SPiece _ c2) -> c2 /= c
            )
        path :: PColor -> Pos -> Int -> Int -> [Pos]
        path c p@(x,y) dx dy = if not (onBoard p')
            then []
            else case b ! p' of
                SEmpty -> p' : path c p' dx dy
                (SPiece _ c2) -> [p' | c /= c2]
            where
                p' = (x+dx, y+dy)
