module View where

import Graphics.Gloss
import Model

window :: Display -- The window that will be displayed.
window = InWindow "My Gloss Game" (400, 300) (10, 10)

background :: Color -- The background color.
background = white

-- Convert a wall into a Picture
wallToPicture :: Wall -> Picture
wallToPicture = positionToRect
  where
    positionToRect (x, y) =
        Translate (x * 200) (y * 150) $ Color black $ rectangleSolid 20 20

-- Convert a food into a Picture
foodToPicture :: Food -> Picture
foodToPicture = positionToCircle
  where
    positionToCircle (x, y) =
        Translate (x * 200) (y * 150) $ Color yellow $ circleSolid 10

-- Convert the game board into a Picture
gameBoardToPicture :: GameBoard -> Picture
gameBoardToPicture (GameBoard walls foods) = 
    pictures $ map wallToPicture walls ++ map foodToPicture foods
