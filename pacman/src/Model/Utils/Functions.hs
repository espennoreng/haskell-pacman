module Model.Utils.Functions where
import Model.Utils.Types as UtilsTypes
import Model.Board.Types
import Model.Pacman.Types
import Model.Constants

import Data.Foldable (minimumBy)
import Data.Function (on)
import Graphics.Gloss

calculateNewPosition :: Position -> Direction -> Position
calculateNewPosition (x, y) proposedDirection =
  case proposedDirection of
    Up -> (x, y + 0.05)
    UtilsTypes.Down -> (x, y - 0.05)
    UtilsTypes.Left -> (x - 0.05, y)
    UtilsTypes.Right -> (x + 0.05, y)

intersects :: Position -> Position -> Bool
intersects (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
      distance = sqrt (dx * dx + dy * dy)
   in distance < moveableEntityRadius

distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) =
  sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

translatePosition :: (Float, Float) -> Picture -> Picture
translatePosition (x, y) = Translate (x * 200) (y * 200)
