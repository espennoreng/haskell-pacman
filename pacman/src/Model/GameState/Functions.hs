module Model.GameState.Functions where

import Model.GameState.Types
import Model.Pacman.Types
import Model.Ghosts.Types
import Model.Board.Types
import Model.Utils.Types as UtilsTypes

import Model.Board.Functions
import Model.Utils.Functions
import Model.Board.GameBoard
import Model.Pacman.Functions
import Model.Ghosts.Functions

import System.Random


resetPacmanAndGhosts :: GameState -> GameState
resetPacmanAndGhosts gameState =
  gameState
    { pacman = initPacman,
      ghosts = initGhosts
    }

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
      powerCookies = makePowerOnEveryCorner pacmanGameBoard,
      ghosts = initGhosts,
      randGen = mkStdGen 0,
      score = 0,
      lives = 3,
      screen = StartScreen,
      paused = False
    }


makeFoodOnEveryAvailablePosition :: GameBoard -> [Food]
makeFoodOnEveryAvailablePosition board =
  let allPositions = [(x, y) | x <- [-0.45, -0.40 .. 0.45], y <- [-0.45, -0.40 .. 0.45]]
   in filter (isPositionFree board) allPositions

makePowerOnEveryCorner :: GameBoard -> [PowerCookie]
makePowerOnEveryCorner board =
  let cornerPositions = [(-0.45, -0.45), (-0.45, 0.45), (0.45, -0.45), (0.45, 0.45)]
   in filter (isPositionFree board) cornerPositions


isPositionFree :: GameBoard -> Position -> Bool
isPositionFree (GameBoard walls) position =
  not (any (intersects position . wallPosition) walls)