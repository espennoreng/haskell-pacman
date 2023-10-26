module View where

import Graphics.Gloss
import Model
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
ghostToPicture (Ghost kind pos _ _ _) = translatePosition pos $ Color (ghostColor kind) $ circleSolid 4
  where
    ghostColor Blinky = red
    ghostColor Pinky  = rose
    ghostColor Inky   = cyan
    ghostColor Clyde  = orange

gameScoreToPicture :: Int -> Picture
gameScoreToPicture score = translate (-100) 110 $ Color white $ Scale 0.1 0.1 $ Text $ "Score: " ++ show score

pacmanLivesToPicture :: Int -> Picture
pacmanLivesToPicture lives = translate (-100) (-120) $ Color white $ Scale 0.1 0.1 $ Text $ "Lives: " ++ show lives

gameBoardToPicture :: GameBoard -> GameState -> Picture
gameBoardToPicture (GameBoard walls) (GameState pacman foods ghosts _ score lives) =
    pictures $ [ wallToPicture wall | wall <- walls ]
            ++ [ foodToPicture food | food <- foods ]
            ++ [ pacmanToPicture pacman ]
            ++ [ ghostToPicture ghost | ghost <- ghosts ]
            ++ [ gameScoreToPicture score ]
            ++ [ pacmanLivesToPicture lives ]
