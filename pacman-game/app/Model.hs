module Model where

-- Mostly board related functions and types.
type Position = (Float, Float) -- Represents a point in the game.

type Vector = (Float, Float) -- Represents a direction and magnitude for movement.

type Wall = Position -- A wall is a position.

type Food = Position -- A food is a position.

data GameBoard = GameBoard
  { walls :: [Wall], -- The walls of the game board.
    foods :: [Food] -- The foods of the game board.
  }
  deriving (Show) -- Make GameBoard printable.

isPositionFree :: GameBoard -> Position -> Bool -- Is the position free of walls and foods?
isPositionFree (GameBoard walls foods) position =
  not (elem position walls || elem position foods)

{-
   I DONT THINK WE NEED DIRECTION HERE
   The 3 functions under here should be checked and explained better
   We dont need to the pacmanRadius (0.1) here, we can just use 0.1 in the functions
-}
pacmanRadius :: Float
pacmanRadius = 0.05

intersects :: Position -> Position -> Bool
intersects (x1, y1) (x2, y2) =
    let dx = x1 - x2
        dy = y1 - y2
        distance = sqrt (dx * dx + dy * dy)
    in distance < pacmanRadius


isPositionFreeOfWalls :: GameBoard -> Pacman -> Direction -> Bool
isPositionFreeOfWalls (GameBoard walls _) position direction =
  let proposedPosition = calculateNewPosition position direction
   in not (any (intersects proposedPosition) walls)


isPositionInBounds :: (Ord a1, Ord a2, Fractional a1, Fractional a2) => (a1, a2) -> Bool
isPositionInBounds (x, y) = x > -0.5 && x < 0.5 && y > -0.5 && y < 0.5

-- Pacman and ghosts related functions and types.
data Direction = Up | Down | Left | Right deriving (Eq, Show)

data Pacman = Pacman
  { position :: Position, -- The position of the pacman.
    direction :: Direction -- The direction of the pacman.
  }
  deriving (Eq, Show)


initPacman :: Pacman -- Initialize the pacman in the center facing right.
initPacman = Pacman {position = (0.0, 0.0), direction = Model.Right}

movePacman ::  GameBoard -> Pacman -> Pacman
movePacman board pacman =
  let proposedPosition = calculateNewPosition pacman (direction pacman)
   in if validMove board pacman (direction pacman)
        then pacman {position = proposedPosition}
        else pacman

validMove :: GameBoard -> Pacman -> Direction -> Bool
validMove board pacman proposedDirection =
  let newPos = calculateNewPosition pacman proposedDirection
   in isPositionFreeOfWalls board pacman proposedDirection

calculateNewPosition :: Pacman -> Direction -> Position
calculateNewPosition pacman proposedDirection =
    let (x, y) = position pacman
     in case proposedDirection of
            Up -> (x, y + 0.1)
            Down -> (x, y - 0.1)
            Model.Left -> (x - 0.1, y)
            Model.Right -> (x + 0.1, y)
