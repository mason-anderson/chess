module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.Array
import qualified Data.Map as M
import System.Directory

import Debug.Trace

import Game
import Data.Maybe

data World = MkWorld
    { textures :: M.Map String Picture
    , dragged :: (Int, Int)
    , turn :: PColor
    , mousePos :: (Float,Float)
    , board :: Board
    }

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

getSquarePicture :: World -> Square -> Picture
getSquarePicture w (SPiece p c) = textures w M.! (show p ++ show c)
getSquarePicture _ SEmpty = blank

getBoardPos :: (Float,Float) -> Maybe (Int,Int)
getBoardPos (x,y) = if bx >= 0 && bx < 8 && by >= 0 && by < 8
                      then Just (bx,by)
                      else Nothing
    where
        bx = round $ (x + windowSize / 2) / squareSize - 1
        by = round $ 9 - (y + windowSize / 2) / squareSize

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

    let moveBoardPos = translate ( fromIntegral file * squareSize)
                                 (-fromIntegral rank * squareSize)
    let square = board w ! (file - 1,rank - 1)
    let squareTex = getSquarePicture w square

    if dragged w == (file - 1,rank - 1)
        then pure blank
        else pure $ moveBoardPos squareTex

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
    case getBoardPos (x,y) of
        Just (bx,by) -> world {dragged = (bx,by)}
        Nothing -> world
handleKeyEvents (EventKey (MouseButton LeftButton) Up _ (x,y)) world = world {
        dragged = (-1,-1),
        board = case getBoardPos (x,y) of
            Just p -> move (dragged world) p $ board world
            Nothing -> board world
        }
handleKeyEvents (EventKey (Char 'p') Down _ _) world = trace (show $ dragged world) world
handleKeyEvents _ world = world

update :: Float -> World -> World
update _ = id

initialState :: World
initialState = MkWorld
    M.empty
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
    print imgs

    let initialState' = initialState { textures = M.fromList imgs}
    play window background fps initialState' render handleKeyEvents update