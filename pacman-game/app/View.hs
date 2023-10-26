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
      Picture(Translate, Color, Pictures), red, rose, orange )
import Model
    ( Food,
      Wall,
      GameBoard(GameBoard),
      Pacman(position),
      GameState(GameState), Ghost (Ghost), GhostType (..) )
window :: Display
window = InWindow "Haskell Pacman" (400, 300) (10, 10)

background :: Color
background = black

-- Translate a position into the corresponding visual space
translatePosition :: (Float, Float) -> Picture -> Picture
translatePosition (x, y) = Translate (x * 200) (y * 200)

wallToPicture :: Wall -> Picture
wallToPicture wall = translatePosition wall $ Pictures [border, wallPic]
  where
    border  = Color white $ rectangleSolid 8 8
    wallPic = Color blue  $ rectangleSolid 7 7

foodToPicture :: Food -> Picture
foodToPicture food = translatePosition food $ Color cyan $ circleSolid 2

pacmanToPicture :: Pacman -> Picture
pacmanToPicture pacman = translatePosition (position pacman) $ Color yellow $ circleSolid 4

ghostToPicture :: Ghost -> Picture
ghostToPicture (Ghost kind pos _ _) = translatePosition pos $ Color (ghostColor kind) $ circleSolid 4
  where
    ghostColor Blinky = red
    ghostColor Pinky  = rose
    ghostColor Inky   = cyan
    ghostColor Clyde  = orange

gameBoardToPicture :: GameBoard -> GameState -> Picture
gameBoardToPicture (GameBoard walls) (GameState pacman foods ghosts) =
    pictures $ [ wallToPicture wall | wall <- walls ]
            ++ [ foodToPicture food | food <- foods ]
            ++ [ pacmanToPicture pacman ]
            ++ [ ghostToPicture ghost | ghost <- ghosts ]
