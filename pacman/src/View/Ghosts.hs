module View.Ghosts where

import Graphics.Gloss
import Model.Ghosts.Types
import Model.Utils.Functions

ghostToPicture :: Ghost -> Picture
ghostToPicture (Ghost kind pos _ _) = translatePosition pos $ Color (ghostColor kind) $ circleSolid 4
  where
    ghostColor Blinky = red
    ghostColor Pinky = rose
    ghostColor Inky = cyan
    ghostColor Clyde = orange
