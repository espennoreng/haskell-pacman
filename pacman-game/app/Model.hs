module Model where

import Data.Foldable (minimumBy)
import Data.List (find)
import Data.Set (Set)

import System.Random
import qualified Data.Set as Set

----------------------------------------
-- Data types
----------------------------------------
-- Score
type Score = Int
-- Board
type Position = (Float, Float)

type Vector = (Float, Float)

type Wall = Position

type Food = Position

newtype GameBoard = GameBoard
  { walls :: [Wall]
  }
  deriving (Eq, Show)

-- Pacman
data Direction = Up | Down | Left | Right deriving (Eq, Show)

data Pacman = Pacman
  { position :: Position, -- The position of the pacman.
    direction :: Direction, -- The direction of the pacman.
    lastSuccessfulDirection :: Direction
  }
  deriving (Eq, Show)

-- Ghost
data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

data GhostMode = Chase | Scatter | Frightened deriving (Eq, Show)

data Ghost = Ghost
  { ghostType :: GhostType,
    ghostPosition :: Position,
    ghostDirection :: Direction,
    ghostMode :: GhostMode,
    ghostLastSuccessfulDirection :: Direction
  }
  deriving (Eq, Show)

-- Game state
data GameState = GameState
  { pacman :: Pacman,
    food :: [Food],
    ghosts :: [Ghost],
    randGen :: StdGen,
    score :: Score
  }
  deriving (Eq, Show)

-- Movable
data Movable a = Movable
  { getPosition :: a -> Position,
    getDirection :: a -> Direction,
    setPosition :: Position -> a -> a,
    getLastSuccessfulDirection :: a -> Direction,
    setLastSuccessfulDirection :: Direction -> a -> a
  }

-- BFS
type Visited = Set.Set Position

----------------------------------------
-- Pacman functions
----------------------------------------
initPacman :: Pacman -- Initialize the pacman in the center facing right.
initPacman = Pacman {position = (0.0, 0.0), direction = Model.Down, lastSuccessfulDirection = Model.Down}

movePacman :: GameBoard -> Pacman -> Pacman
movePacman = moveEntity pacmanMovable

pacmanMovable :: Movable Pacman
pacmanMovable =
  Movable
    { getPosition = position,
      getDirection = direction,
      setPosition = \newPos p -> p {position = newPos},
      getLastSuccessfulDirection = lastSuccessfulDirection,
      setLastSuccessfulDirection = \dir p -> p {lastSuccessfulDirection = dir}
    }

----------------------------------------
-- Ghost functions
----------------------------------------
initGhosts :: [Ghost]
initGhosts =
  [ Ghost {ghostType = Blinky, ghostPosition = (0.0, 0.10), ghostDirection = Model.Up, ghostMode = Chase, ghostLastSuccessfulDirection = Model.Up},
    Ghost {ghostType = Pinky, ghostPosition = (0.0, 0.10), ghostDirection = Model.Down, ghostMode = Chase, ghostLastSuccessfulDirection = Model.Down},
    Ghost {ghostType = Inky, ghostPosition = (-0.05, 0.10), ghostDirection = Model.Up, ghostMode = Chase, ghostLastSuccessfulDirection = Model.Up},
    Ghost {ghostType = Clyde, ghostPosition = (0.05, 0.10), ghostDirection = Model.Up, ghostMode = Chase, ghostLastSuccessfulDirection = Model.Up}
  ]

getBlinkyTarget :: Pacman -> Position
getBlinkyTarget = position

getPinkyTarget :: Pacman -> Position
getPinkyTarget pacman = validMoveAhead 4 (direction pacman) (position pacman)

getInkyTarget :: Pacman -> Position -> Position
getInkyTarget pacman blinkyPos =
  let twoAheadOfPacman = validMoveAhead 2 (direction pacman) (position pacman)
      deltaX = fst twoAheadOfPacman - fst blinkyPos
      deltaY = snd twoAheadOfPacman - snd blinkyPos
   in (fst twoAheadOfPacman + 2 * deltaX, snd twoAheadOfPacman + 2 * deltaY)

moveInky :: GameBoard -> Pacman -> Ghost -> [Ghost] -> Ghost
moveInky board pacman inky allGhosts =
  let blinkyPos = ghostPosition (findGhost Blinky allGhosts)
      target = getInkyTarget pacman blinkyPos
      newDirection = getNextMoveBFS board inky target
   in moveEntity ghostMovable board inky {ghostDirection = newDirection}

moveBlinky :: GameBoard -> Pacman -> Ghost -> Ghost
moveBlinky board pacman blinky =
  if ghostMode blinky == Chase
    then moveEntity ghostMovable board blinky {ghostDirection = getNextMoveBFS board blinky (getBlinkyTarget pacman)}
    else blinky

movePinky :: GameBoard -> Pacman -> Ghost -> Ghost
movePinky board pacman pinky =
  if ghostMode pinky == Chase
    then moveEntity ghostMovable board pinky {ghostDirection = getNextMoveBFS board pinky (getPinkyTarget pacman)}
    else pinky

shouldChase :: StdGen -> (Bool, StdGen)
shouldChase gen =
  let (n, newGen) = randomR (1 :: Int, 10) gen
   in (n > 5, newGen)

moveClyde :: StdGen -> GameBoard -> Pacman -> Ghost -> (Ghost, StdGen)
moveClyde gen board pacman clyde =
  let (shouldChasePacman, newGen1) = shouldChase gen
      (movedClyde, newGen2)
        | shouldChasePacman = (moveEntity ghostMovable board clyde {ghostDirection = getNextMoveBFS board clyde (getBlinkyTarget pacman)}, newGen1)
        | otherwise = moveClydeRandomly newGen1 board clyde
   in (movedClyde, newGen2)

moveClydeRandomly :: StdGen -> GameBoard -> Ghost -> (Ghost, StdGen)
moveClydeRandomly gen board clyde =
  let (n, newGen) = randomR (1 :: Int, 4) gen
      newDirection = case n of
        1 -> Up
        2 -> Down
        3 -> Model.Left
        4 -> Model.Right
        _ -> error "Invalid random number"
   in (moveEntity ghostMovable board clyde {ghostDirection = newDirection}, newGen)

moveGhosts :: StdGen -> GameBoard -> Pacman -> [Ghost] -> ([Ghost], StdGen)
moveGhosts gen board pacman ghosts =
  let blinky = findGhost Blinky ghosts
      pinky = findGhost Pinky ghosts
      inky = findGhost Inky ghosts
      clyde = findGhost Clyde ghosts
      (newClyde, newGen) = moveClyde gen board pacman clyde
   in ([moveBlinky board pacman blinky, movePinky board pacman pinky, moveInky board pacman inky ghosts, newClyde], newGen)

ghostMovable :: Movable Ghost
ghostMovable =
  Movable
    { getPosition = ghostPosition,
      getDirection = ghostDirection,
      setPosition = \newPos g -> g {ghostPosition = newPos},
      getLastSuccessfulDirection = ghostLastSuccessfulDirection,
      setLastSuccessfulDirection = \dir g -> g {ghostLastSuccessfulDirection = dir}
    }

findGhost :: GhostType -> [Ghost] -> Ghost
findGhost _ [] = error "No ghosts found"
findGhost gType (ghost : rest) =
  if ghostType ghost == gType
    then ghost
    else findGhost gType rest

----------------------------------------
-- Board functions
----------------------------------------

initGameState :: GameState
initGameState =
  GameState
    { pacman = initPacman,
      food = makeFoodOnEveryAvailablePosition pacmanGameBoard,
      ghosts = initGhosts,
      randGen = mkStdGen 0,
      score = 0
    }

isPositionInBounds :: (Ord a1, Ord a2, Fractional a1, Fractional a2) => (a1, a2) -> Bool
isPositionInBounds (x, y) = x > -0.5 && x < 0.5 && y > -0.5 && y < 0.5

makeFoodOnEveryAvailablePosition :: GameBoard -> [Food]
makeFoodOnEveryAvailablePosition board =
  let allPositions = [(x, y) | x <- [-0.45, -0.40 .. 0.45], y <- [-0.45, -0.40 .. 0.45]]
   in filter (isPositionFree board) allPositions

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

-- A board with walls on each side and a line in the middle.
testBoard :: GameBoard
testBoard =
  GameBoard
    { walls =
        -- Left side
        [(-0.5, y) | y <- [-0.5, -0.45 .. 0.5]]
          -- Top side
          ++ [(x, 0.5) | x <- [-0.5, -0.45 .. 0.5]]
          -- Right side
          ++ [(0.5, y) | y <- [-0.5, -0.45 .. 0.5]]
          -- Bottom side
          ++ [(x, -0.5) | x <- [-0.5, -0.45 .. 0.5]]
          -- Some internal vertical walls
          ++ [(-0.3, y) | y <- [0.0, 0.1, 0.2]]
          ++ [(0.3, y) | y <- [-0.2, -0.1, 0.0]]
    }

----------------------------------------
-- Helper functions
----------------------------------------

validMove :: GameBoard -> Position -> Direction -> Bool
validMove board position proposedDirection =
  let newPos = calculateNewPosition position proposedDirection
   in isPositionFreeOfWalls board newPos proposedDirection

validMoveAhead :: Int -> Direction -> Position -> Position
validMoveAhead 0 _ pos = pos
validMoveAhead n dir pos = validMoveAhead (n - 1) dir (calculateNewPosition pos dir)

calculateNewPosition :: Position -> Direction -> Position
calculateNewPosition (x, y) proposedDirection =
  case proposedDirection of
    Up -> (x, y + 0.05)
    Model.Down -> (x, y - 0.05)
    Model.Left -> (x - 0.05, y)
    Model.Right -> (x + 0.05, y)

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

isPositionFree :: GameBoard -> Position -> Bool
isPositionFree board position =
  isPositionInBounds position && not (any (intersects position) (walls board))

bfs :: GameBoard -> Position -> Position -> [Direction]
bfs board start target = bfsHelper board [(start, [])] target Set.empty

bfsHelper :: GameBoard -> [(Position, [Direction])] -> Position -> Visited -> [Direction]
bfsHelper board [] _ _ = [] -- Empty list means no path found.
bfsHelper board ((currentPosition, path) : rest) target visited
  | currentPosition == target = path
  | otherwise =
      let neighbors = filter (`Set.notMember` visited) $ adjacentPositions board currentPosition
          newVisited = Set.insert currentPosition visited
          updatedPaths = [(pos, path ++ [directionForMove currentPosition pos]) | pos <- neighbors]
       in bfsHelper board (rest ++ updatedPaths) target newVisited

adjacentPositions :: GameBoard -> Position -> [Position]
adjacentPositions board pos =
  [newPos | dir <- [Up, Down, Model.Left, Model.Right], isPositionFreeOfWalls board pos dir, let newPos = calculateNewPosition pos dir]

directionForMove :: Position -> Position -> Direction
directionForMove (x1, y1) (x2, y2)
  | y2 > y1 = Up
  | y2 < y1 = Down
  | x2 > x1 = Model.Right
  | x2 < x1 = Model.Left
  | otherwise = error "Invalid move"

getNextMoveBFS :: GameBoard -> Ghost -> Position -> Direction
getNextMoveBFS board blinky target =
  case bfs board (ghostPosition blinky) target of
    [] -> ghostDirection blinky -- No path found, keep current direction.
    (dir : _) -> dir

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

----------------------------------------
-- Constants
----------------------------------------

pacmanAndGhostRadius :: Float
pacmanAndGhostRadius = 0.025
