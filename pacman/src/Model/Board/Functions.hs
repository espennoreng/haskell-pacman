module Model.Board.Functions where
import Model.Board.Types
import Model.Utils.Types
import Model.Pacman.Types

import Model.Utils.Functions


wallPosition :: Wall -> Position
wallPosition (GhostHomeWall pos) = pos
wallPosition (NormalWall pos) = pos

isPositionFreeOfWalls :: GameBoard -> Position -> Direction -> Bool
isPositionFreeOfWalls (GameBoard walls) position direction =
  let proposedPosition = calculateNewPosition position direction
   in not (any (intersects proposedPosition . wallPosition) walls)

moveEntity :: Movable a -> GameBoard -> a -> a
moveEntity movable board entity =
  let proposedPosition = calculateNewPosition (getPosition movable entity) (getDirection movable entity)
   in if isPositionFreeOfWalls board (getPosition movable entity) (getDirection movable entity)
        then setPosition movable proposedPosition (setLastSuccessfulDirection movable (getDirection movable entity) entity)
        else
          let fallbackPosition = calculateNewPosition (getPosition movable entity) (getLastSuccessfulDirection movable entity)
           in if isPositionFreeOfWalls board (getPosition movable entity) (getLastSuccessfulDirection movable entity)
                then setPosition movable fallbackPosition entity
                else entity

validMove :: GameBoard -> Position -> Direction -> Bool
validMove board position proposedDirection =
  let newPos = calculateNewPosition position proposedDirection
   in isPositionFreeOfWalls board newPos proposedDirection

validMoveAhead :: Int -> Direction -> Position -> Position
validMoveAhead 0 _ pos = pos
validMoveAhead n dir pos = validMoveAhead (n - 1) dir (calculateNewPosition pos dir)
