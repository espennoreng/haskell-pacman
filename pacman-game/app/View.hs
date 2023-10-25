module View where

import Graphics.Gloss
    ( black,
      blue,
      cyan,
      white,
      yellow,
      pictures,
      rectangleSolid,
      circleSolid,
      Display(InWindow),
      Color,
      Picture(Translate, Color, Pictures) )
import Model
    ( Food,
      Wall,
      GameBoard(GameBoard),
      Pacman(position),
      GameState(GameState) )

window :: Display -- The window that will be displayed.
window = InWindow "Haskell Pacman" (400, 300) (10, 10)

background :: Color -- The background color.
background = black

-- Convert a wall into a Picture

wallToPicture :: Wall -> Picture
wallToPicture = positionToRect
  where
    positionToRect (x, y) =
        Translate (x * 200) (y * 200) $ Pictures [border, wall]
      where
        border = Color white $ rectangleSolid 8 8  -- slightly larger for the border
        wall   = Color blue  $ rectangleSolid 7 7  -- original wall


-- Convert a food into a Picture
foodToPicture :: Food -> Picture
foodToPicture = positionToCircle
  where
    positionToCircle (x, y) =
        Translate (x * 200) (y * 200) $ Color cyan $ circleSolid 2

-- Convert pacman into a Picture
pacmanToPicture :: Pacman -> Picture
pacmanToPicture pacman =
    let (x, y) = position pacman 
    in Translate (x * 200) (y * 200) $ Color yellow $ circleSolid 4


-- Convert the game board with walls, foods and pacman into a Picture
gameBoardToPicture :: GameBoard -> GameState -> Picture
gameBoardToPicture (GameBoard walls) (GameState pacman food) =
    pictures $ map wallToPicture walls  ++ map foodToPicture food ++ [pacmanToPicture pacman]
