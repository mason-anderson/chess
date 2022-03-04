module Game where

import Data.Array
import Data.List
import Data.Bifunctor

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

move :: Pos -> Pos -> Board -> Board
move i f b = if f `elem` legalSquares
    then b // [(i, SEmpty), (f, b ! i)]
    else b
    where
        piece = b ! i
        legalSquares :: [Pos]
        legalSquares = case piece of
            -- (SPiece Pawn c) -> [new_pos | b ! new_pos == SEmpty]
                -- ++ [f new_pos | f <- [first (+1), first (subtract 1)] , getColor (b ! f new_pos) == Just White]
                    -- where new_pos = (if c == White then second (+1) else second (subtract 1)) i
            (SPiece Pawn c) -> filter (legal c) [bimap (+1) dir i, bimap (subtract 1) dir i] ++ [x | x <- [second dir i], onBoard x && b ! x == SEmpty]
                where dir = if c == White then (+1) else subtract 1

            (SPiece Bishop c) -> concat [path c i x y | x <- [-1, 1], y <- [-1, 1]]
            (SPiece Knight c) -> undefined
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
