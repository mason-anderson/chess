module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.Array
import qualified Data.Map as M
import System.Directory

import Game
import Data.Maybe

data World = MkWorld
    { textures :: M.Map String Picture
    , bottomColor :: PColor
    , dragged :: (Int, Int)
    , turn :: PColor
    , mousePos :: (Float,Float)
    , board :: Board
    }

-- make (0,0) the top left of the screen
translateTopLeft :: Picture -> Picture
translateTopLeft = translate (-windowSize / 2) (windowSize / 2)

lightBlue = makeColorI 0x8c 0xa2 0xad 255

imageFolder = "./res"

windowSize :: Num a => a
windowSize = 1000
offset = 100
background = makeColorI 100 100 100 0
fps = 60
window = InWindow "Chess" (windowSize, windowSize) (offset, offset)

squareSize = 100

-- get the Picture for the current square (a piece or empty)
getSquarePicture :: World -> Square -> Picture
getSquarePicture w (SPiece p c) = textures w M.! (show p ++ show c)
getSquarePicture _ SEmpty = blank

getBoardPos :: PColor -> (Float,Float) -> Maybe (Int,Int)
getBoardPos bottomColor (x,y) = if bx >= 0 && bx < 8 && by >= 0 && by < 8
    then Just (bx,by)
    else Nothing
    where
        bx = round $ (x + windowSize / 2) / squareSize - 1
        by = if bottomColor == White
            then round $ (y + windowSize / 2) / squareSize - 2
            else round $ 9 - (y + windowSize / 2) / squareSize

drawBoard :: Picture
drawBoard = pictures $ do
    file <- [1..8]
    rank <- [1..8]

    let moveBoardPos = translate (file * squareSize) (-rank * squareSize)
    let square = rectangleSolid squareSize squareSize
    let square' = moveBoardPos square

    pure $ if even $ round $ file + rank
        then color white square'
        else color lightBlue square'

drawPieces :: World -> Picture
drawPieces w = pictures $ do
    file <- [1..8]
    rank <- [1..8]

    let moveBoardPos = if bottomColor w == White
        then translate ( fromIntegral file * squareSize)
                       (-fromIntegral (9 - rank) * squareSize)
        else translate ( fromIntegral file * squareSize)
                       (-fromIntegral rank * squareSize)

    let square = board w ! (file - 1,rank - 1)
    let squareTex = getSquarePicture w square

    if dragged w == (file - 1,rank - 1)
        then pure blank
        else pure $ moveBoardPos squareTex

-- draw the piece being dragged by the mouse
drawDragged :: World -> Picture
drawDragged w = if dragged w == (-1,-1)
    then blank
    else translate (x + windowSize / 2) (y - windowSize / 2) tex
    where
        tex = getSquarePicture w $ board w ! dragged w
        (x,y) = mousePos w

render :: World -> Picture
render w = translateTopLeft $ pictures
    [ drawBoard
    , drawPieces w
    , drawDragged w
    ]

handleKeyEvents :: Event -> World -> World
handleKeyEvents (EventMotion p) world = world { mousePos = p }
handleKeyEvents (EventKey (MouseButton LeftButton) Down _ (x,y)) world =
    case (board world !) <$> getBoardPos (bottomColor world) (x,y) of
        Just (SPiece _ c) -> if c == turn world
            then world {dragged = fromJust $ getBoardPos (bottomColor world) (x,y)}
            else world
        _ -> world
-- handleKeyEvents (EventKey (MouseButton LeftButton) Up _ (x,y)) world = if dragged world /= (-1,-1)
--     then world
--         { dragged = (-1,-1)
--         , board = case getBoardPos (bottomColor world) (x,y) of
--             Just p -> if legalMove (dragged world) p ((0,0),(0,0)) $ board world
--                 then board world // [(dragged world, SEmpty), (p, board world ! (dragged world))]
--                 else board world
--             Nothing -> board world
--         , turn = if turn world == White then Black else White
--         }
--     else world
handleKeyEvents (EventKey (MouseButton LeftButton) Up _ (x,y)) world = if dragged world /= (-1,-1)
    then case getBoardPos (bottomColor world) (x,y) of
        Just fPos -> if legalMove iPos fPos ((0,0),(0,0)) b
            then world {board = board world // [(dragged world, SEmpty), (fPos, board world ! (dragged world))], turn = if turn world == White then Black else White, dragged = (-1,-1)}
            else world {dragged = (-1,-1)}
    else world {dragged = (-1,-1)}
    where
        b = board world
        iPos = dragged world
handleKeyEvents (EventKey (Char 'p') Down _ _) world = world
handleKeyEvents _ world = world

update :: Float -> World -> World
update _ = id

initialState :: World
initialState = MkWorld
    M.empty
    White
    (-1,-1)
    White
    (0,0)
    initialBoard

main :: IO ()
main = do
    imgPaths <- getDirectoryContents imageFolder
    let f x = do
            img <- loadJuicyPNG x
            case img of
                Nothing -> pure Nothing
                Just img -> pure $ Just (drop 4 $ reverse $ drop 4 $ reverse x,img)
    imgs <- catMaybes <$> mapM (f . ("res/" ++)) imgPaths

    let initialState' = initialState { textures = M.fromList imgs}
    play window background fps initialState' render handleKeyEvents update
