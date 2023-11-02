module View.Walls where

import Graphics.Gloss
import Model.Board.Types
import Model.Board.Functions
import Model.Utils.Functions

wallToPicture :: Wall -> Picture
wallToPicture wall = translatePosition (wallPosition wall) $ Pictures [border, wallPic]
  where
    border = case wall of
      GhostHomeWall _ -> Color white $ rectangleSolid 4 4
      NormalWall _ -> Color white $ rectangleSolid 9 9
    wallPic = case wall of
      GhostHomeWall _ -> Color blue $ rectangleSolid 2 2
      NormalWall _ -> Color blue $ rectangleSolid 8 8
