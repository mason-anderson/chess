module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.Array
import qualified Data.Map as M
import System.Directory
import Data.Maybe

import Game

-- the state of the whole game
data World = MkWorld
    { textures :: M.Map String Picture
    , bottomColor :: PColor
    , dragged :: (Int, Int)
    , turn :: PColor
    , mousePos :: (Float,Float)
    , gameState :: GameState
    }

board = gsBoard . gameState

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

-- return the coordinate of the square a position is in
-- flip horizontally and vertically if black is on the bottom
getBoardPos :: PColor -> (Float,Float) -> Maybe (Int,Int)
getBoardPos bottomColor (x,y) = if bx >= 0 && bx < 8 && by >= 0 && by < 8
    then Just (bx,by)
    else Nothing
    where
        bx = if bottomColor == White
            then round $ (x + windowSize / 2) / squareSize - 1
            else round $ 9 - (x + windowSize / 2) / squareSize - 1
        by = if bottomColor == White
            then round $ (y + windowSize / 2) / squareSize - 2
            else round $ 9 - (y + windowSize / 2) / squareSize

-- draw the board underneath the pieces
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

-- draw the pieces
drawPieces :: World -> Picture
drawPieces w = pictures $ do
    file <- [1..8]
    rank <- [1..8]

    let moveBoardPos = if bottomColor w == White
        then translate ( fromIntegral file * squareSize)
                       (-fromIntegral (9 - rank) * squareSize)
        else translate ( fromIntegral (9 - file) * squareSize)
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
handleKeyEvents (EventKey (MouseButton LeftButton) Up _ (x,y)) world = if dragged world /= (-1,-1)
    then case getBoardPos (bottomColor world) (x,y) of
        Just fPos -> if legalMove iPos fPos gs
            then world
            { gameState = makeMove iPos fPos gs
            , turn = if turn world == White then Black else White
            , dragged = (-1,-1)
            }
            else world {dragged = (-1,-1)}
        Nothing -> world {dragged = (-1,-1)}
    else world
    where
        b = board world
        gs = gameState world
        iPos = dragged world
handleKeyEvents (EventKey (Char 'f') Down _ _) world = world {bottomColor = oppositeColor (bottomColor world)}
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
    initialGameState

main :: IO ()
main = do
    -- load images of pieces
    imgPaths <- getDirectoryContents imageFolder
    let f x = do
            let imgName = drop 4 $ reverse $ drop 4 $ reverse x
            img <- loadJuicyPNG x
            case img of
                Nothing -> pure Nothing
                Just img -> pure $ Just (imgName,scale 1.3 1.3 img)
    imgs <- catMaybes <$> mapM (f . ("res/" ++)) imgPaths

    let initialState' = initialState { textures = M.fromList imgs}
    play window background fps initialState' render handleKeyEvents update
