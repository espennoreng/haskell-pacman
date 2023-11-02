module Model.Ai.Functions where

import Data.Function (on)
import Data.Foldable (minimumBy)
import Model.Board.Types
import Model.Utils.Types as UtilsTypes

import Model.Utils.Functions
import Model.Board.Functions


greedyMove :: GameBoard -> Position -> Position -> Position
greedyMove board currentPos targetPos =
  let possibleDirections = [Up, Down, UtilsTypes.Left, UtilsTypes.Right]
      possiblePositions =
        [calculateNewPosition currentPos dir | dir <- possibleDirections, isPositionFreeOfWalls board currentPos dir]
      closestPosition = minimumBy (compare `on` distance targetPos) possiblePositions
   in closestPosition