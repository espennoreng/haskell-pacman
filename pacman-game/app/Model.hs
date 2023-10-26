module Model where

-- Mostly board related functions and types.
type Position = (Float, Float) -- Represents a point in the game.

type Vector = (Float, Float) -- Represents a direction and magnitude for movement.

type Wall = Position -- A wall is a position.

type Food = Position -- A food is a position.

-- Pacman and ghosts related functions and types.
data Direction = Up | Down | Left | Right deriving (Eq, Show)

data Pacman = Pacman
  { position :: Position, -- The position of the pacman.
    direction :: Direction -- The direction of the pacman.
  }
  deriving (Eq, Show)

data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

data Ghost = Ghost
  { ghostType :: GhostType, -- The type of the ghost.
    ghostPosition :: Position, -- The position of the ghost.
    ghostDirection :: Direction -- The direction of the ghost.
  }
  deriving (Eq, Show)

data GameState = GameState
  { pacman :: Pacman, -- The pacman.
    food :: [Food], -- The food.
    ghosts :: [Ghost] -- The ghosts.
  }
  deriving (Eq, Show)

newtype GameBoard = GameBoard
  { walls :: [Wall] -- The walls of the game board.
  }
  deriving (Eq, Show)

initGhosts :: [Ghost]
initGhosts =
  [ Ghost {ghostType = Blinky, ghostPosition = (0.0, 0.10), ghostDirection = Model.Up},
    Ghost {ghostType = Pinky, ghostPosition = (0.0, 0.10), ghostDirection = Model.Down},
    Ghost {ghostType = Inky, ghostPosition = (-0.05, 0.10), ghostDirection = Model.Up},
    Ghost {ghostType = Clyde, ghostPosition = (0.05, 0.10), ghostDirection = Model.Up}
  ]

pacmanAndGhostRadius :: Float
pacmanAndGhostRadius = 0.025

intersects :: Position -> Position -> Bool
intersects (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
      distance = sqrt (dx * dx + dy * dy)
   in distance < pacmanAndGhostRadius

isPositionFreeOfWalls :: GameBoard -> Position -> Direction -> Bool
isPositionFreeOfWalls (GameBoard walls) position direction =
  let proposedPosition = calculateNewPosition position direction
   in not (any (intersects proposedPosition) walls)

isPositionInBounds :: (Ord a1, Ord a2, Fractional a1, Fractional a2) => (a1, a2) -> Bool
isPositionInBounds (x, y) = x > -0.5 && x < 0.5 && y > -0.5 && y < 0.5

initGameState :: GameState
initGameState =
  GameState
    { pacman = initPacman,
      food = makeFoodOnEveryAvailablePosition pacmanGameBoard,
      ghosts = initGhosts
    }

initPacman :: Pacman -- Initialize the pacman in the center facing right.
initPacman = Pacman {position = (0.0, 0.0), direction = Model.Right}

-- TODO: Make sure that food is not placed inside the ghost house.
makeFoodOnEveryAvailablePosition :: GameBoard -> [Food]
makeFoodOnEveryAvailablePosition board =
  let allPositions = [(x, y) | x <- [-0.45, -0.40 .. 0.45], y <- [-0.45, -0.40 .. 0.45]]
   in filter (isPositionFree board) allPositions

isPositionFree :: GameBoard -> Position -> Bool
isPositionFree board position =
  isPositionInBounds position && not (any (intersects position) (walls board))

-- Define a data type to encapsulate the logic of movement-related fields (EXPLAIN THIS BETTER)
-- Here there where some equal logic so we created a data type to encapsulate it
data Movable a = Movable
  { getPosition     :: a -> Position
  , getDirection    :: a -> Direction
  , setPosition     :: Position -> a -> a
  }

moveEntity :: Movable a -> GameBoard -> a -> a
moveEntity movable board entity =
  let proposedPosition = calculateNewPosition (getPosition movable entity) (getDirection movable entity)
   in if isPositionFreeOfWalls board (getPosition movable entity) (getDirection movable entity)
        then setPosition movable proposedPosition entity
        else entity

-- Create instances of Movable for Pacman and Ghost
pacmanMovable :: Movable Pacman
pacmanMovable = Movable
  { getPosition     = position
  , getDirection    = direction
  , setPosition     = \newPos p -> p {position = newPos}
  }

ghostMovable :: Movable Ghost
ghostMovable = Movable
  { getPosition     = ghostPosition
  , getDirection    = ghostDirection
  , setPosition     = \newPos g -> g {ghostPosition = newPos}
  }

-- New functions using the generalized moveEntity function
movePacman :: GameBoard -> Pacman -> Pacman
movePacman = moveEntity pacmanMovable

moveGhost :: GameBoard -> Ghost -> Ghost
moveGhost = moveEntity ghostMovable


validMove :: GameBoard -> Position -> Direction -> Bool
validMove board position proposedDirection =
  let newPos = calculateNewPosition position proposedDirection
   in isPositionFreeOfWalls board position proposedDirection

calculateNewPosition :: Position -> Direction -> Position
calculateNewPosition (x, y) proposedDirection =
  case proposedDirection of
    Up -> (x, y + 0.05)
    Down -> (x, y - 0.05)
    Model.Left -> (x - 0.05, y)
    Model.Right -> (x + 0.05, y)

pacmanGameBoard :: GameBoard
pacmanGameBoard =
  GameBoard
    { walls =
        outerWalls
          ++ leftBottomL
          ++ rightBottomL
          ++ bottomU
          ++ leftBottomZ
          ++ rightBottomZ
          ++ bottomT
          ++ leftMiddleU
          ++ rightMiddleU
          ++ leftLineMiddle
          ++ rightLineMiddle
          ++ ghostHouseWalls
          ++ middleLeftRectangle
          ++ middleRightRectangle
          ++ topLeftL
          ++ topRightL
          ++ middleRectangleTopLeft
          ++ middleRectangleTopRight
          ++ topT
          ++ topZLeft
          ++ topZRight
    }
  where
    outerWalls =
      [(-0.5, y) | y <- [-0.5, -0.45 .. 0.5]] -- Left side
        ++ [(x, 0.5) | x <- [-0.5, -0.45 .. 0.5]] -- Top side
        ++ [(0.5, y) | y <- [-0.5, -0.45 .. 0.5]] -- Right side
        ++ [(x, -0.5) | x <- [-0.5, -0.45 .. 0.5]] -- Bottom side
    leftBottomL =
      [(-0.4, y) | y <- [-0.4, -0.35, -0.3]] -- vertical
        ++ [(x, -0.4) | x <- [-0.4, -0.35 .. -0.20]] -- horizontal
    rightBottomL =
      [(0.4, y) | y <- [-0.4, -0.35, -0.3]] -- vertical
        ++ [(x, -0.4) | x <- [0.20, 0.25 .. 0.4]] -- horizontal
    bottomU =
      [(x, -0.4) | x <- [-0.10, -0.05 .. 0.10]] -- horizontal
        ++ [(0.10, y) | y <- [-0.4, -0.35 .. -0.25]] -- vertical left side
        ++ [(-0.10, y) | y <- [-0.4, -0.35 .. -0.25]] -- vertical right side
        ++ [(-0.0, y) | y <- [-0.30, -0.25]] -- vertical middle
    leftBottomZ =
      [(x, -0.30) | x <- [-0.30, -0.25, -0.20]] -- horizontal bottom part
        ++ [(x, -0.15) | x <- [-0.20, -0.15, -0.10]] -- horizontal top part
        ++ [(-0.20, y) | y <- [-0.3, -0.25, -0.20]] -- vertical left part
    rightBottomZ =
      [(x, -0.30) | x <- [0.20, 0.25, 0.30]] -- horizontal bottom part
        ++ [(x, -0.15) | x <- [0.10, 0.15, 0.20]] -- horizontal top part
        ++ [(0.20, y) | y <- [-0.3, -0.25, -0.20]] -- vertical right part
    bottomT =
      [(-0.0, y) | y <- [-0.05, -0.10, -0.15]] -- Horizontal part of T
        ++ [(x, -0.05) | x <- [-0.10, -0.05 .. 0.10]] -- Vertical part of T
    leftMiddleU =
      [(x, -0.20) | x <- [-0.4, -0.35, -0.30]] -- Horizontal bottom part of U
        ++ [(-0.30, y) | y <- [-0.20, -0.15 .. 0.0]] -- Vertical left part of U
        ++ [(x, 0.0) | x <- [-0.30, -0.35, -0.40]] -- Horizontal top part of U
        ++ [(x, -0.10) | x <- [-0.4, -0.45]] -- Island in the middle of U
    rightMiddleU =
      [(x, -0.20) | x <- [0.4, 0.35, 0.30]] -- Horizontal bottom part of U
        ++ [(0.30, y) | y <- [-0.20, -0.15 .. 0.0]] -- Vertical right part of U
        ++ [(x, 0.0) | x <- [0.30, 0.35, 0.40]] -- Horizontal top part of U
        ++ [(x, -0.10) | x <- [0.4, 0.45]] -- Island in the middle of U
    leftLineMiddle =
      [(-0.20, y) | y <- [-0.05, 0.0 .. 0.15]] -- Vertical left part of line
    rightLineMiddle =
      [(0.20, y) | y <- [-0.05, 0.0 .. 0.15]] -- Vertical right part of line
    ghostHouseWalls =
      [(x, 0.05) | x <- [-0.10, -0.05 .. 0.10]] -- Horizontal part of ghost house
        ++ [(0.10, y) | y <- [0.05, 0.10, 0.15]] -- Vertical left part of ghost house
        ++ [(-0.10, y) | y <- [0.05, 0.10, 0.15]] -- Vertical right part of ghost house
        ++ [(x, 0.15) | x <- [-0.05, 0.05]] -- Horizontal top part of ghost house
    middleLeftRectangle =
      [(x, 0.15) | x <- [-0.30, -0.35, -0.40]] -- Horizontal top part of U
        ++ [(x, 0.10) | x <- [-0.30, -0.35, -0.40]] -- Horizontal top part of U
    middleRightRectangle =
      [(x, 0.15) | x <- [0.30, 0.35, 0.40]] -- Horizontal top part of U
        ++ [(x, 0.10) | x <- [0.30, 0.35, 0.40]] -- Horizontal top part of U
    topLeftL =
      [(-0.4, y) | y <- [0.25, 0.30 .. 0.40]] -- Vertical left part of L
        ++ [(x, 0.4) | x <- [-0.4, -0.35, -0.30]] -- Horizontal top part of L
    topRightL =
      [(0.4, y) | y <- [0.25, 0.30 .. 0.40]] -- Vertical right part of L
        ++ [(x, 0.4) | x <- [0.4, 0.35, 0.30]] -- Horizontal top part of L
    middleRectangleTopLeft =
      [(x, 0.25) | x <- [-0.30, -0.25, -0.20]] -- Horizontal part of line
        ++ [(x, 0.30) | x <- [-0.30, -0.25, -0.20]] -- Horizontal top part of line
    middleRectangleTopRight =
      [(x, 0.25) | x <- [0.20, 0.25, 0.30]] -- Horizontal part of line
        ++ [(x, 0.30) | x <- [0.30, 0.25, 0.20]] -- Horizontal top part of line
    topT =
      [(x, 0.25) | x <- [-0.10, -0.05 .. 0.10]] -- Horizontal part of T
        ++ [(0.0, y) | y <- [0.25, 0.30 .. 0.40]] -- Vertical part of T
    topZLeft =
      [(x, 0.40) | x <- [-0.20, -0.15, -0.10]] -- Horizontal middle part
        ++ [(x, 0.45) | x <- [-0.20]] -- Horizontal top part
        ++ [(x, 0.35) | x <- [-0.10]] -- Horizontal bottom part
    topZRight =
      [(x, 0.40) | x <- [0.20, 0.15, 0.10]] -- Horizontal middle part
        ++ [(x, 0.45) | x <- [0.20]] -- Horizontal top part
        ++ [(x, 0.35) | x <- [0.10]] -- Horizontal bottom part

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
          ++ [(x, -0.1) | x <- [0.1, 0.2, 0.3]]
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
          ++ [(x, -0.5) | x <- [-0.5, -0.4 .. 0.5]]
    }
