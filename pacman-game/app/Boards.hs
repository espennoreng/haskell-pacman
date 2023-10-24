module Boards where
import Model (Food, GameBoard (..), Wall, isPositionFree, isPositionInBounds)

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
          ++ [(x, -0.5) | x <- [-0.5, -0.4 .. 0.5]],
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
