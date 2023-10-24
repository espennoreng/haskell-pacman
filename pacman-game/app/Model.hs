module Model where

type Position = (Float, Float)  -- Represents a point in the game.

type Vector = (Float, Float)    -- Represents a direction and magnitude for movement.
 
type Wall = Position            -- A wall is a position.

type Food = Position            -- A food is a position.

data GameBoard = GameBoard
  { walls :: [Wall],            -- The walls of the game board.
    foods :: [Food]             -- The foods of the game board.
  }
  deriving (Show)               -- Make GameBoard printable.

isPositionFree :: GameBoard -> Position -> Bool  -- Is the position free of walls and foods?
isPositionFree (GameBoard walls foods) position =
    not (elem position walls || elem position foods)

isPositionInBounds :: Position -> Bool -- Is the position within the bounds of the game board?
isPositionInBounds (x, y) = abs x <= 0.5 && abs y <= 0.5