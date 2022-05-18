module Game where

import Data.Array
import Data.List
import Data.Bifunctor
import Data.Maybe

import Debug.Trace

data PColor = White | Black
    deriving (Eq, Show)
data Piece = Pawn | Bishop | Knight | Rook | King | Queen
    deriving (Eq, Show)
data Square = SPiece Piece PColor | SEmpty
    deriving (Eq, Show)

type Board = (Array (Int,Int) Square)

type Pos = (Int,Int)
newtype PiecePos = UnsafeMkPiecePos Pos

data GameState = MkGameState
    { gsBoard :: Board
    , gsLastMove :: (Pos,Pos)
    , gsKingMoved :: (Bool,Bool)
    , gsKRookMoved :: (Bool,Bool)
    , gsQRookMoved :: (Bool,Bool)
    }

getColor :: Square -> Maybe PColor
getColor SEmpty = Nothing
getColor (SPiece _ c) = Just c

oppositeColor :: PColor -> PColor
oppositeColor White = Black
oppositeColor Black = White

onBoard :: Pos -> Bool
onBoard (x,y) = x >= 0 && x < 8 && y >= 0 && y < 8

initialGameState :: GameState
initialGameState = MkGameState
    { gsBoard = initialBoard
    , gsLastMove = ((0,0),(0,0))
    , gsKingMoved = (False,False)
    , gsKRookMoved = (False,False)
    , gsQRookMoved = (False,False)
    }

-- >>> initialBoard ! (3,0)
-- SPiece Queen White

board2 :: Board
board2 = listArray ((0,0),(7,7)) $ concat $ transpose
    [ [SPiece Rook White, SEmpty, SEmpty, SEmpty, SPiece King White, SEmpty, SEmpty, SEmpty]
    , map ($ White) pawns
    , empty
    , empty
    , empty
    , empty
    , map ($ Black) pawns
    , [SPiece Rook Black, SEmpty, SEmpty, SEmpty, SPiece King Black, SEmpty, SEmpty, SEmpty]
    ]
    where
        empty = SEmpty <$ [1..8]
        pawns = SPiece Pawn <$ [1..8]


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

checkMate :: GameState -> Maybe PColor
checkMate b = undefined

-- makes the move doing extra work for special moves like castling or taking en passant
makeMove :: Pos -> Pos -> GameState -> GameState
makeMove iPos fPos gs = if piece == King && (abs dx > 1) -- check for castle
    then gs {gsBoard = board // [(iPos, SEmpty)
                                , (fPos, board ! iPos)
                                , (rookPos, SEmpty)
                                , ((if dx > 0 then fst fPos - 1 else fst fPos + 1,snd fPos), SPiece Rook color)
                                ] } -- castle move
    else gs {gsBoard = board // [(iPos, SEmpty), (fPos, board ! iPos)]} -- regular move
    where
        piece = case board ! iPos of SPiece p _ -> p
        color = case board ! iPos of SPiece _ c -> c
        dx = fst fPos - fst iPos
        rookPos = (if dx > 0 then 7 else 0, snd iPos)
        board = gsBoard gs

-- check if move is legal (move is possible and doesn't leave player in check at the end)
legalMove :: Pos -> Pos -> GameState -> Bool
legalMove iPos fPos gs = movePossible iPos fPos gs && not (inCheck color (makeMove iPos fPos gs))
    where
        color = fromJust $ getColor (gsBoard gs ! iPos)

-- return True if that color's king is in check
-- we do this by checking to see if the king's pos is a legal move for any pieces
inCheck :: PColor -> GameState -> Bool
inCheck color gs = any (\p -> movePossible p kingPos gs) oppositeColorPiecesPos
    where
        board = gsBoard gs
        kingPos = fst $ head $ filterKing $ assocs board
        filterKing = filter (\case (_,SPiece King c) -> c == color; _ -> False)

        oppositeColorPiecesPos = map fst $ filterOppColor $ assocs board
        filterOppColor = filter (\case (_, SPiece _ c) -> c /= color; _ -> False)

-- checks if a move is possible (not including moving in check)
movePossible :: Pos -> Pos -> GameState -> Bool
movePossible i f gs = f `elem` legalSquares
    where
        b = gsBoard gs
        piece = b ! i
        -- return a list of legal squares for the piece at i
        legalSquares :: [Pos]
        legalSquares = case piece of
            (SPiece Pawn c) -> filter (isLegalTake c) [ bimap (+1) (dir 1) i, bimap (subtract 1) (dir 1) i] -- take on diagonals
                            ++ moveOne -- move 1
                            ++ [x | x<-[second (dir 2) i], not (null moveOne) && b ! x == SEmpty && (snd i == 1 || snd i == 6)] -- move 2 (first move only)
                -- dir returns the direction of movement for the correct color
                where dir x = if c == White then (+x) else subtract x
                      moveOne = [x | x<-[second (dir 1) i], onBoard x && b ! x == SEmpty]
                      isLegalTake c x = onBoard x && (case b ! x of
                                SEmpty -> False
                                (SPiece _ c2) -> c2 /= c
                          )

            (SPiece Bishop c) -> concat [path c i x y | x <- [-1, 1], y <- [-1, 1]]
            (SPiece Knight c) -> filter (legal c) [(fst i +x,snd i + y) | x<-[1,-1,2,-2], y<-[1,-1,2,-2], abs x  /= abs y]
            (SPiece Rook c) -> concat [bPath 1 0, bPath (-1) 0, bPath 0 1, bPath 0 (-1)]
                where bPath = path c i
            (SPiece King c) -> trace (show canShortCastle) $ filter (legal c) [(fst i + x,snd i + y) | x <-[-1,0,1], y <- [-1,0,1], x /= 0 || y /= 0]
                            ++ [(fst i + 2, snd i) | canShortCastle] -- short castle
                            ++ [(fst i - 2, snd i) | canLongCastle] -- long castle
                where
                    canShortCastle =
                        -- check if king or rook has moved
                        (if c == White
                            then not $ fst (gsKingMoved gs) || fst (gsKRookMoved gs)
                            else not $ snd (gsKingMoved gs) || snd (gsKRookMoved gs)
                        )&& -- check if pieces are in the way
                        and [(b ! (x, snd i)) == SEmpty | x <- [5,6]]
                    canLongCastle =
                        -- check if king or rook has moved
                        (if c == White
                            then not $ fst (gsKingMoved gs) || fst (gsQRookMoved gs)
                            else not $ snd (gsKingMoved gs) || snd (gsQRookMoved gs)
                        )&& -- check if pieces are in the way
                        and [(b ! (x, snd i)) == SEmpty | x <- [1,2,3]]
            (SPiece Queen c) -> concat [path c i x y | x <- [-1,0,1], y <- [-1,0,1], x /= 0 || y /= 0]
            _ -> []

        -- checks if legal (ignoring check)
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
