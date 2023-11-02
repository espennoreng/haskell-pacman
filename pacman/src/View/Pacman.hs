module View.Pacman where

import Graphics.Gloss
import Model.Pacman.Types
import Model.Utils.Functions

pacmanToPicture :: Pacman -> Picture
pacmanToPicture pacman = translatePosition (position pacman) $ Color yellow $ circleSolid 4

pacmanLivesToPicture :: Int -> Picture
pacmanLivesToPicture lives = translate (-100) (-120) $ Color white $ Scale 0.1 0.1 $ Text $ "Lives: " ++ show lives
