module Boards where
import Model (Food, GameBoard (..), Wall, isPositionFree, isPositionInBounds)

pacmanGameBoard :: GameBoard
pacmanGameBoard =
  GameBoard
    { walls = outerWalls ++ 
              leftBottomL ++ 
              rightBottomL ++ 
              bottomU ++
              leftBottomZ ++
              rightBottomZ ++
              bottomT ++
              leftMiddleU ++
              rightMiddleU ++
              leftLineMiddle ++
              rightLineMiddle ++
              ghostHouseWalls ++
              middleLeftRectangle ++
              middleRightRectangle ++
              topLeftL ++
              topRightL ++
              middleRectangleTopLeft ++
              middleRectangleTopRight ++
              topT ++ 
              topZLeft ++
              topZRight
        ,
      foods = []
    }
    where 
      outerWalls = 
          [(-0.5, y) | y <- [-0.5, -0.45 .. 0.5]]         -- Left side
          ++ [(x, 0.5) | x <- [-0.5, -0.45 .. 0.5]]       -- Top side
          ++ [(0.5, y) | y <- [-0.5, -0.45 .. 0.5]]       -- Right side
          ++ [(x, -0.5) | x <- [-0.5, -0.45 .. 0.5]]      -- Bottom side
      leftBottomL = 
          [(-0.4, y) | y <- [-0.4, -0.35, -0.3]]          -- vertical
          ++ [(x, -0.4) | x <- [-0.4, -0.35 .. -0.20]]    -- horizontal
      rightBottomL = 
          [(0.4, y) | y <- [-0.4, -0.35, -0.3]]           -- vertical
          ++ [(x, -0.4) | x <- [0.20, 0.25 .. 0.4]]       -- horizontal
      bottomU =
          [(x, -0.4) | x <- [-0.10, -0.05 .. 0.10]]       -- horizontal
          ++ [(0.10, y) | y <- [-0.4, -0.35 .. -0.25]]    -- vertical left side
          ++ [(-0.10, y) | y <- [-0.4, -0.35 .. -0.25]]   -- vertical right side
          ++ [(-0.0, y) | y <- [-0.30, -0.25]]            -- vertical middle
      leftBottomZ = 
          [(x, -0.30) | x <- [-0.30, -0.25, -0.20]]       -- horizontal bottom part
          ++ [(x, -0.15) | x <- [-0.20, -0.15, -0.10]]    -- horizontal top part
          ++ [(-0.20, y) | y <- [-0.3, -0.25, -0.20]]     -- vertical left part
      rightBottomZ = 
          [(x, -0.30) | x <- [0.20, 0.25, 0.30]]          -- horizontal bottom part
          ++ [(x, -0.15) | x <- [0.10, 0.15, 0.20]]       -- horizontal top part
          ++ [(0.20, y) | y <- [-0.3, -0.25, -0.20]]      -- vertical right part
      bottomT =
        [(-0.0, y) | y <- [-0.05, -0.10, -0.15]]          -- Horizontal part of T
        ++ [(x, -0.05) | x <- [-0.10, -0.05 .. 0.10]]     -- Vertical part of T
      leftMiddleU =
        [(x, -0.20) | x <- [-0.4, -0.35, -0.30]]          -- Horizontal bottom part of U
        ++ [(-0.30, y) | y <- [-0.20, -0.15 .. 0.0]]      -- Vertical left part of U
        ++ [(x, 0.0) | x <- [-0.30, -0.35, -0.40]]        -- Horizontal top part of U
        ++ [(x, -0.10) | x <- [-0.4, -0.45]]              -- Island in the middle of U
      rightMiddleU =
        [(x, -0.20) | x <- [0.4, 0.35, 0.30]]             -- Horizontal bottom part of U
        ++ [(0.30, y) | y <- [-0.20, -0.15 .. 0.0]]       -- Vertical right part of U
        ++ [(x, 0.0) | x <- [0.30, 0.35, 0.40]]           -- Horizontal top part of U
        ++ [(x, -0.10) | x <- [0.4, 0.45]]                -- Island in the middle of U
      leftLineMiddle =
        [(-0.20, y) | y <- [-0.05, 0.0 .. 0.15]]          -- Vertical left part of line
      rightLineMiddle =
        [(0.20, y) | y <- [-0.05, 0.0 .. 0.15]]           -- Vertical right part of line
      ghostHouseWalls =
        [(x, 0.05) | x <- [-0.10, -0.05 .. 0.10]]         -- Horizontal part of ghost house
        ++ [(0.10, y) | y <- [0.05, 0.10, 0.15]]          -- Vertical left part of ghost house
        ++ [(-0.10, y) | y <- [0.05, 0.10, 0.15]]         -- Vertical right part of ghost house
        ++ [(x, 0.15) | x <- [-0.05, 0.05]]               -- Horizontal top part of ghost house
      middleLeftRectangle =
        [(x, 0.15) | x <- [-0.30, -0.35, -0.40]]          -- Horizontal top part of U
        ++ [(x, 0.10) | x <- [-0.30, -0.35, -0.40]]       -- Horizontal top part of U
      middleRightRectangle =
        [(x, 0.15) | x <- [0.30, 0.35, 0.40]]             -- Horizontal top part of U
        ++ [(x, 0.10) | x <- [0.30, 0.35, 0.40]]          -- Horizontal top part of U
      topLeftL =
        [(-0.4, y) | y <- [0.25, 0.30 .. 0.40]]           -- Vertical left part of L
        ++ [(x, 0.4) | x <- [-0.4, -0.35, -0.30]]         -- Horizontal top part of L
      topRightL =
        [(0.4, y) | y <- [0.25, 0.30 .. 0.40]]            -- Vertical right part of L
        ++ [(x, 0.4) | x <- [0.4, 0.35, 0.30]]            -- Horizontal top part of L
      middleRectangleTopLeft =
        [(x, 0.25) | x <- [-0.30, -0.25, -0.20]]          -- Horizontal part of line
        ++ [(x, 0.30) | x <- [-0.30, -0.25, -0.20]]       -- Horizontal top part of line
      middleRectangleTopRight =
        [(x, 0.25) | x <- [0.20, 0.25, 0.30]]             -- Horizontal part of line
        ++ [(x, 0.30) | x <- [0.30, 0.25, 0.20]]          -- Horizontal top part of line
      topT =
        [(x, 0.25) | x <- [-0.10, -0.05 .. 0.10]]         -- Horizontal part of T
        ++ [(0.0, y) | y <- [0.25, 0.30 .. 0.40]]         -- Vertical part of T
      topZLeft =
        [(x, 0.40) | x <- [-0.20, -0.15, -0.10]]          -- Horizontal middle part
        ++ [(x, 0.45) | x <- [-0.20]]                     -- Horizontal top part
        ++ [(x, 0.35 ) | x <- [-0.10]]                    -- Horizontal bottom part
      topZRight =
        [(x, 0.40) | x <- [0.20, 0.15, 0.10]]             -- Horizontal middle part
        ++ [(x, 0.45) | x <- [0.20]]                      -- Horizontal top part
        ++ [(x, 0.35) | x <- [0.10]]                      -- Horizontal bottom part

validGameBoard :: GameBoard
validGameBoard =
  GameBoard
    { walls =
        -- Left side
        [(-0.5, y) | y <- [-0.5, -0.4 .. 0.5]]
        -- Top side
        ++ [(x, 0.5) | x <- [-0.5, -0.4 .. 0.5]]
        -- Right side
        ++ [(0.5, y) | y <- [-0.5, -0.4 .. 0.5]]
        -- Bottom side
        ++ [(x, -0.5) | x <- [-0.5, -0.4 .. 0.5]]
        -- Some internal vertical walls
        ++ [(-0.3, y) | y <- [0.0, 0.1, 0.2]]
        ++ [(0.3, y) | y <- [-0.2, -0.1, 0.0]]
        -- Some internal horizontal walls
        ++ [(x, 0.3) | x <- [-0.2, -0.1, 0.0]]
        ++ [(x, -0.1) | x <- [0.1, 0.2, 0.3]],
      foods = [(0.0, 0.0)] -- Single food in the center for now
    }


invalidGameBoard :: GameBoard
invalidGameBoard =
  GameBoard
    { walls =
        -- Left side
        [(-0.5, y) | y <- [-0.5, -0.4 .. 0.5]]
          -- Top side
          ++ [(x, 0.5) | x <- [-0.5, -0.4 .. 0.5]]
          -- Right side
          ++ [(0.5, y) | y <- [-0.5, -0.4 .. 0.5]]
          -- Bottom side
          ++ [(x, -0.5) | x <- [-0.5, -0.4 .. 0.5]],
      foods = [(-0.5, -0.5), (0.5, 0.5)] -- Foods placed in the corners (inside walls)
    }

