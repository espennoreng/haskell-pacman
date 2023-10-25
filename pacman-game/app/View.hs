module View where

import Graphics.Gloss
import Model

window :: Display -- The window that will be displayed.
window = InWindow "My Gloss Game" (400, 300) (10, 10)

background :: Color -- The background color.
background = greyN 0.5

-- Convert a wall into a Picture
wallToPicture :: Wall -> Picture
wallToPicture = positionToRect
  where
    positionToRect (x, y) =
        Translate (x * 200) (y * 200) $ Color black $ rectangleSolid 20 20

-- Convert a food into a Picture
foodToPicture :: Food -> Picture
foodToPicture = positionToCircle
  where
    positionToCircle (x, y) =
        Translate (x * 200) (y * 200) $ Color blue $ rectangleSolid 20 20

-- Convert pacman into a Picture
pacmanToPicture :: Pacman -> Picture
pacmanToPicture pacman =
    let (x, y) = position pacman 
    in Translate (x * 200) (y * 200) $ Color yellow $ rectangleSolid 20 20


-- Convert the game board with walls, foods and pacman into a Picture
gameBoardToPicture :: GameBoard -> Pacman -> Picture
gameBoardToPicture (GameBoard walls foods) pacman =
    pictures $ map wallToPicture walls ++ map foodToPicture foods ++ [pacmanToPicture pacman]
