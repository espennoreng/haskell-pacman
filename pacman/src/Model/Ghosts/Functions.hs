module Model.Ghosts.Functions where

import Model.Ai.Functions
import Model.Board.Functions
import Model.Board.Types
import Model.Constants
import Model.Ghosts.Types
import Model.Pacman.Types
import Model.Utils.Functions
import Model.Utils.Types as UtilsTypes
import System.Random
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad


initGhosts :: [Ghost]
initGhosts =
  [ Ghost
      { ghostType = Blinky,
        ghostPosition = (0.0, 0.10),
        releaseTimer = 0.0,
        ghostMode = Chase,
        frightenedTimer = 0.0
      },
    Ghost
      { ghostType = Pinky,
        ghostPosition = (0.0, 0.10),
        releaseTimer = 3.0,
        ghostMode = Chase,
        frightenedTimer = 0.0
      },
    Ghost
      { ghostType = Inky,
        ghostPosition = (0.0, 0.10),
        releaseTimer = 6.0,
        ghostMode = Chase,
        frightenedTimer = 0.0
      },
    Ghost
      { ghostType = Clyde,
        ghostPosition = (0.0, 0.10),
        releaseTimer = 9.0,
        ghostMode = Chase,
        frightenedTimer = 0.0
      }
  ]

placeGhostsInGhostHouse :: [Ghost] -> [Ghost]
placeGhostsInGhostHouse ghosts =
  [ghost {ghostPosition = (0.0, 0.10), releaseTimer = initialTimer (ghostType ghost)} | ghost <- ghosts]
  where
    initialTimer :: GhostType -> Float
    initialTimer Blinky = 0 -- Blinky is released immediately
    initialTimer Pinky = 3 -- Pinky is released after 5 seconds
    initialTimer Inky = 6 -- Inky after 10 seconds
    initialTimer Clyde = 9 -- Clyde after 15 seconds

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

moveInky :: GameBoard -> Pacman -> Ghost -> [Ghost] -> Maybe Ghost
moveInky board pacman inky allGhosts =
  case findGhost Blinky allGhosts of
    Just blinky ->
      let blinkyPos = ghostPosition blinky
          target = getInkyTarget pacman blinkyPos
          newPos = greedyMove board (ghostPosition inky) target
      in Just $ inky {ghostPosition = newPos}
    Nothing -> Nothing


moveClyde :: StdGen -> GameBoard -> Pacman -> Ghost -> (Ghost, StdGen)
moveClyde gen board pacman clyde =
  let (shouldMoveRandomly, newGen) = shouldChase gen
   in if shouldMoveRandomly
        then moveGhostRandomly gen board clyde
        else
          let target = position pacman
              newPos = greedyMove board (ghostPosition clyde) target
           in (clyde {ghostPosition = newPos}, newGen)

shouldChase :: StdGen -> (Bool, StdGen)
shouldChase gen =
  let (n, newGen) = randomR (1 :: Int, 10) gen
   in (n > 5, newGen)

ensureAllGhostsPresent :: [Ghost] -> [Ghost]
ensureAllGhostsPresent ghosts = 
  let existingGhostTypes = map ghostType ghosts
      requiredGhostTypes = [Blinky, Pinky, Inky, Clyde]
      missingGhostTypes = filter (`notElem` existingGhostTypes) requiredGhostTypes
      spawnedGhosts = map spawnGhost missingGhostTypes
  in ghosts ++ spawnedGhosts
  where
    spawnGhost :: GhostType -> Ghost
    spawnGhost gType = Ghost
      { ghostType = gType,
        ghostPosition = (0.0, 0.10),
        releaseTimer = 3.0,
        ghostMode = Chase,
        frightenedTimer = 0.0
      }

    initialTimer :: GhostType -> Float
    initialTimer Blinky = 0
    initialTimer Pinky = 3
    initialTimer Inky = 6
    initialTimer Clyde = 9

moveGhosts :: StdGen -> GameBoard -> Pacman -> [Ghost] -> ([Ghost], StdGen)
moveGhosts gen board pacman ghosts =
  if frightenedTimer (head ghosts) > 0
    then moveGhostsRandomly gen board ghosts
    else moveGhostsChasing gen board pacman ghosts

    
moveGhostsChasing :: StdGen -> GameBoard -> Pacman -> [Ghost] -> ([Ghost], StdGen)
moveGhostsChasing gen board pacman ghosts =
  let blinky = findGhost Blinky ghosts
      pinky = findGhost Pinky ghosts
      inky = findGhost Inky ghosts
      clyde = findGhost Clyde ghosts
      movedGhosts = catMaybes [ moveBlinky board pacman <$> blinky,
                               movePinky board pacman <$> pinky,
                               join (moveInky board pacman <$> inky <*> pure ghosts),
                               fst <$> (moveClyde gen board pacman <$> clyde) ]
      newGen = maybe gen (snd . moveClyde gen board pacman) clyde
  in (movedGhosts, newGen)


moveGhostsRandomly :: StdGen -> GameBoard -> [Ghost] -> ([Ghost], StdGen)
moveGhostsRandomly gen board ghosts =
  let blinky = findGhost Blinky ghosts
      pinky = findGhost Pinky ghosts
      inky = findGhost Inky ghosts
      clyde = findGhost Clyde ghosts
      (newBlinky, gen1) = maybe (head ghosts, gen) (moveGhostRandomly gen board) blinky
      (newPinky, gen2) = maybe (head ghosts, gen1) (moveGhostRandomly gen1 board) pinky
      (newInky, gen3) = maybe (head ghosts, gen2) (moveGhostRandomly gen2 board) inky
      (newClyde, gen4) = maybe (head ghosts, gen3) (moveGhostRandomly gen3 board) clyde
  in ([newBlinky, newPinky, newInky, newClyde], gen4)

moveGhostRandomly :: StdGen -> GameBoard -> Ghost -> (Ghost, StdGen)
moveGhostRandomly gen board ghost =
  let (n, newGen) = randomR (1 :: Int, 4) gen
      newDirection = case n of
        1 -> Up
        2 -> Down
        3 -> UtilsTypes.Left
        4 -> UtilsTypes.Right
        _ -> error "Invalid random number"
      proposedPosition = calculateNewPosition (ghostPosition ghost) newDirection
   in if isPositionFreeOfWalls board (ghostPosition ghost) newDirection
        then (ghost {ghostPosition = proposedPosition}, newGen)
        else (ghost, newGen)

findGhost :: GhostType -> [Ghost] -> Maybe Ghost
findGhost _ [] = Nothing
findGhost gType (ghost : rest) =
  if ghostType ghost == gType
  then Just ghost
  else findGhost gType rest


updateGhostTimer :: Float -> Ghost -> Ghost
updateGhostTimer deltaTime ghost =
  if releaseTimer ghost > 0
    then ghost {releaseTimer = releaseTimer ghost - deltaTime}
    else ghost


setFrightenedTimer :: [Ghost] -> [Ghost]
setFrightenedTimer ghosts =
  [ghost {frightenedTimer = frightenedTime} | ghost <- ghosts]
  where
    frightenedTime :: Float
    frightenedTime = 10.0

decrementFrightenedTimer :: Float -> Ghost -> Ghost
decrementFrightenedTimer deltaTime ghost =
  if frightenedTimer ghost > 0
    then ghost {frightenedTimer = frightenedTimer ghost - deltaTime}
    else ghost

moveGhostToExitPosition :: GameBoard -> Ghost -> Position -> Ghost
moveGhostToExitPosition board ghost exitPosition =
  if isPositionFreeOfWallsGhostEdition board (ghostPosition ghost) exitPosition
    then ghost {ghostPosition = exitPosition, ghostMode = Chase}
    else ghost

isPositionFreeOfWallsGhostEdition :: GameBoard -> Position -> Position -> Bool
isPositionFreeOfWallsGhostEdition (GameBoard walls) currentPos proposedPos =
  not $ any matchesPosition walls
  where
    matchesPosition :: Wall -> Bool
    matchesPosition (NormalWall pos) = pos == proposedPos
    matchesPosition (GhostHomeWall pos) = pos == proposedPos

moveGhostOutOfGhostHouse :: GameBoard -> Ghost -> Ghost
moveGhostOutOfGhostHouse board ghost =
  case ghostType ghost of
    Blinky -> moveGhostToExitPosition board ghost (0.0, 0.20)
    Pinky -> moveGhostToExitPosition board ghost (0.0, 0.20)
    Inky -> moveGhostToExitPosition board ghost (0.0, 0.20)
    Clyde -> moveGhostToExitPosition board ghost (0.0, 0.20)

isInsideGhostHouse :: Position -> Bool
isInsideGhostHouse (x, y) = x >= -0.10 && x <= 0.10 && y >= 0.05 && y <= 0.15

releaseGhostIfNeeded :: GameBoard -> Ghost -> Ghost
releaseGhostIfNeeded board ghost =
  if releaseTimer ghost <= 0 && isInsideGhostHouse (ghostPosition ghost)
    then moveGhostOutOfGhostHouse board ghost
    else ghost