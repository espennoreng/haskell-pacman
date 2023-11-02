module Model where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (find)
import System.Random
import Model.Pacman.Types
import Model.Ghosts.Types
import Model.Board.Types
import Model.Utils.Types as UtilsTypes

----------------------------------------
-- Data types
----------------------------------------
-- Board

type Score = Int

type Lives = Int

type Food = Position

data GameScreen = StartScreen | GameScreen | GameOverScreen deriving (Eq, Show)

-- Game state
type Paused = Bool

data GameState = GameState
  { pacman :: Pacman,
    food :: [Food],
    ghosts :: [Ghost],
    randGen :: StdGen,
    score :: Score,
    lives :: Lives,
    screen :: GameScreen,
    paused :: Paused
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

----------------------------------------
-- Pacman functions
----------------------------------------
initPacman :: Pacman -- Initialize the pacman in the center facing right.
initPacman = Pacman {position = (-0.45, -0.45), direction = UtilsTypes.Down, lastSuccessfulDirection = UtilsTypes.Down}

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

pacmanEatsFood :: Pacman -> [Food] -> (Bool, [Food])
pacmanEatsFood pacman food =
  let pacmanPos = position pacman
      foodEaten = find (intersects pacmanPos) food
   in case foodEaten of
        Just f -> (True, filter (/= f) food)
        Nothing -> (False, food)

pacmanLoosesLife :: Pacman -> [Ghost] -> Lives -> Lives
pacmanLoosesLife pacman ghosts lives =
  if any (intersects (position pacman) . ghostPosition) ghosts
    then
      if lives > 0
        then lives - 1
        else 0
    else lives

resetPacmanAndGhosts :: GameState -> GameState
resetPacmanAndGhosts gameState =
  gameState
    { pacman = initPacman,
      ghosts = initGhosts
    }

----------------------------------------
-- Ghost functions
----------------------------------------
initGhosts :: [Ghost]
initGhosts =
  [ Ghost {ghostType = Blinky, ghostPosition = (0.0, 0.10), releaseTimer = 0.0},
    Ghost {ghostType = Pinky, ghostPosition = (0.0, 0.10) , releaseTimer = 3.0},
    Ghost {ghostType = Inky, ghostPosition = (0.0, 0.10) , releaseTimer = 6.0},
    Ghost {ghostType = Clyde, ghostPosition = (0.0, 0.10) , releaseTimer = 9.0}
  ]

placeGhostsInGhostHouse :: [Ghost] -> [Ghost]
placeGhostsInGhostHouse ghosts =
  [ghost {ghostPosition = (0.0, 0.10), releaseTimer = initialTimer (ghostType ghost)} | ghost <- ghosts]
  where
    initialTimer :: GhostType -> Float
    initialTimer Blinky = 0      -- Blinky is released immediately
    initialTimer Pinky = 3       -- Pinky is released after 5 seconds
    initialTimer Inky = 6      -- Inky after 10 seconds
    initialTimer Clyde = 9     -- Clyde after 15 seconds

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

moveBlinky :: GameBoard -> Pacman -> Ghost -> Ghost
moveBlinky board pacman blinky =
  let target = getBlinkyTarget pacman
      newPos = greedyMove board (ghostPosition blinky) target
   in blinky {ghostPosition = newPos}

movePinky :: GameBoard -> Pacman -> Ghost -> Ghost
movePinky board pacman pinky =
  let target = getPinkyTarget pacman
      newPos = greedyMove board (ghostPosition pinky) target
   in pinky {ghostPosition = newPos}

moveInky :: GameBoard -> Pacman -> Ghost -> [Ghost] -> Ghost
moveInky board pacman inky allGhosts =
  let blinkyPos = ghostPosition $ findGhost Blinky allGhosts
      target = getInkyTarget pacman blinkyPos
      newPos = greedyMove board (ghostPosition inky) target
   in inky {ghostPosition = newPos}

moveClyde :: StdGen -> GameBoard -> Pacman -> Ghost -> (Ghost, StdGen)
moveClyde gen board pacman clyde =
  let (shouldMoveRandomly, newGen) = shouldChase gen
   in if shouldMoveRandomly
        then moveClydeRandomly gen board clyde
        else
          let target = position pacman
              newPos = greedyMove board (ghostPosition clyde) target
           in (clyde {ghostPosition = newPos}, newGen)

shouldChase :: StdGen -> (Bool, StdGen)
shouldChase gen =
  let (n, newGen) = randomR (1 :: Int, 10) gen
   in (n > 5, newGen)

moveClydeRandomly :: StdGen -> GameBoard -> Ghost -> (Ghost, StdGen)
moveClydeRandomly gen board clyde =
  let (n, newGen) = randomR (1 :: Int, 4) gen
      newDirection = case n of
        1 -> Up
        2 -> Down
        3 -> UtilsTypes.Left
        4 -> UtilsTypes.Right
        _ -> error "Invalid random number"
      proposedPosition = calculateNewPosition (ghostPosition clyde) newDirection
   in if isPositionFreeOfWalls board (ghostPosition clyde) newDirection
        then (clyde {ghostPosition = proposedPosition}, newGen)
        else (clyde, newGen)

moveGhosts :: StdGen -> GameBoard -> Pacman -> [Ghost] -> ([Ghost], StdGen)
moveGhosts gen board pacman ghosts =
  let blinky = findGhost Blinky ghosts
      pinky = findGhost Pinky ghosts
      inky = findGhost Inky ghosts
      clyde = findGhost Clyde ghosts
      (newClyde, newGen) = moveClyde gen board pacman clyde
   in ([moveBlinky board pacman blinky, movePinky board pacman pinky, moveInky board pacman inky ghosts, newClyde], newGen)

findGhost :: GhostType -> [Ghost] -> Ghost
findGhost _ [] = error "No ghosts found"
findGhost gType (ghost : rest) =
  if ghostType ghost == gType
    then ghost
    else findGhost gType rest

updateGhostTimer :: Float -> Ghost -> Ghost
updateGhostTimer deltaTime ghost =
  if releaseTimer ghost > 0
    then ghost {releaseTimer = releaseTimer ghost - deltaTime}
    else ghost

moveGhostToExitPosition :: GameBoard -> Ghost -> Position -> Ghost
moveGhostToExitPosition board ghost exitPosition =
  if isPositionFreeOfWallsGhostEdition board (ghostPosition ghost) exitPosition
    then ghost {ghostPosition = exitPosition, ghostMode = Chase} -- Update mode if needed
    else ghost

-- We assume isPositionFreeOfWalls checks if a given position is not blocked by a wall.
-- It could look something like this (but you'll need to use your existing function or write one):
isPositionFreeOfWallsGhostEdition :: GameBoard -> Position -> Position -> Bool
isPositionFreeOfWallsGhostEdition (GameBoard walls) currentPos proposedPos =
  not $ any (\wall -> case wall of
                         NormalWall pos -> pos == proposedPos
                         GhostHomeWall pos -> pos == proposedPos) walls

-- Adjust the 'moveGhostOutOfGhostHouse' to pass the 'GameBoard' argument along:
moveGhostOutOfGhostHouse :: GameBoard -> Ghost -> Ghost
moveGhostOutOfGhostHouse board ghost =
  case ghostType ghost of
    Blinky -> moveGhostToExitPosition board ghost (0.0, 0.20)
    Pinky -> moveGhostToExitPosition board ghost (0.0, 0.20)
    Inky -> moveGhostToExitPosition board ghost (0.0, 0.20)
    Clyde -> moveGhostToExitPosition board ghost (0.0, 0.20)

-- Keep the isInsideGhostHouse function as it was:
isInsideGhostHouse :: Position -> Bool
isInsideGhostHouse (x, y) = x >= -0.10 && x <= 0.10 && y >= 0.05 && y <= 0.15

-- Now, let's update the 'releaseGhostIfNeeded' to use the 'isInsideGhostHouse' check:
releaseGhostIfNeeded :: GameBoard -> Ghost -> Ghost
releaseGhostIfNeeded board ghost =
  if releaseTimer ghost <= 0 && isInsideGhostHouse (ghostPosition ghost)
    then moveGhostOutOfGhostHouse board ghost
    else ghost
----------------------------------------
-- Board functions
----------------------------------------

wallPosition :: Wall -> Position
wallPosition (GhostHomeWall pos) = pos
wallPosition (NormalWall pos) = pos

restartGame :: GameState -> GameState
restartGame gameState =
  gameState
    { pacman = initPacman,
      food = makeFoodOnEveryAvailablePosition pacmanGameBoard,
      ghosts = initGhosts,
      randGen = mkStdGen 0,
      score = 0,
      lives = 3,
      screen = StartScreen,
      paused = False
    }

resetPositionsAndLooseLife :: GameState -> GameState
resetPositionsAndLooseLife gameState =
  gameState
    { pacman = initPacman,
      ghosts = initGhosts,
      lives = lives gameState - 1
    }

initGameState :: GameState
initGameState =
  GameState
    { pacman = initPacman,
      food = makeFoodOnEveryAvailablePosition pacmanGameBoard,
      ghosts = initGhosts,
      randGen = mkStdGen 0,
      score = 0,
      lives = 3,
      screen = StartScreen,
      paused = False
    }

isPositionInBounds :: (Ord a1, Ord a2, Fractional a1, Fractional a2) => (a1, a2) -> Bool
isPositionInBounds (x, y) = x > -0.5 && x < 0.5 && y > -0.5 && y < 0.5

makeFoodOnEveryAvailablePosition :: GameBoard -> [Food]
makeFoodOnEveryAvailablePosition board =
  let allPositions = [(x, y) | x <- [-0.45, -0.40 .. 0.45], y <- [-0.45, -0.40 .. 0.45]]
   in filter (isPositionFree board) allPositions

ghostHomeWalls :: [Wall]
ghostHomeWalls =
  [GhostHomeWall (x, 0.05) | x <- [-0.10, -0.05 .. 0.10]] -- Horizontal part of ghost house
    ++ [GhostHomeWall (0.10, y) | y <- [0.05, 0.10, 0.15]] -- Vertical left part of ghost house
    ++ [GhostHomeWall (-0.10, y) | y <- [0.05, 0.10, 0.15]] -- Vertical right part of ghost house
    ++ [GhostHomeWall (x, 0.15) | x <- [-0.10, -0.05 .. 0.10]] -- Horizontal top part of ghost house

normalWalls :: [Wall]
normalWalls =
  map NormalWall $ concat
    [ outerWalls
    , leftBottomL
    , rightBottomL
    , bottomU
    , leftBottomZ
    , rightBottomZ
    , bottomT
    , leftMiddleU
    , rightMiddleU
    , leftLineMiddle
    , rightLineMiddle
    , middleLeftRectangle
    , middleRightRectangle
    , topLeftL
    , topRightL
    , middleRectangleTopLeft
    , middleRectangleTopRight
    , topT
    , topZLeft
    , topZRight
    ]
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

pacmanGameBoard :: GameBoard
pacmanGameBoard =
  GameBoard
    { walls = ghostHomeWalls ++ normalWalls
    }

----------------------------------------
-- Helper functions
----------------------------------------

-- Calculate the Euclidean distance between two positions
distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) =
  sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- Calculate the next position for a ghost using a greedy algorithm
greedyMove :: GameBoard -> Position -> Position -> Position
greedyMove board currentPos targetPos =
  let possibleDirections = [Up, Down, UtilsTypes.Left, UtilsTypes.Right]
      possiblePositions =
        [calculateNewPosition currentPos dir | dir <- possibleDirections, isPositionFreeOfWalls board currentPos dir]
      closestPosition = minimumBy (compare `on` distance targetPos) possiblePositions
   in closestPosition

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
    UtilsTypes.Down -> (x, y - 0.05)
    UtilsTypes.Left -> (x - 0.05, y)
    UtilsTypes.Right -> (x + 0.05, y)

intersects :: Position -> Position -> Bool
intersects (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
      distance = sqrt (dx * dx + dy * dy)
   in distance < pacmanAndGhostRadius

isPositionFreeOfWalls :: GameBoard -> Position -> Direction -> Bool
isPositionFreeOfWalls (GameBoard walls) position direction =
  let proposedPosition = calculateNewPosition position direction
   in not (any (intersects proposedPosition . wallPosition) walls)

isPositionFree :: GameBoard -> Position -> Bool
isPositionFree (GameBoard walls) position =
  isPositionInBounds position && not (any (intersects position . wallPosition) walls)

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
