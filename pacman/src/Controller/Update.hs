module Controller.Update where

import Controller.HighScore
import Model.Board.GameBoard
import Model.GameState.Functions
import Model.GameState.Types
import Model.Ghosts.Functions
import Model.Pacman.Functions

handleUpdate :: Float -> GameState -> IO GameState
handleUpdate deltaTime gameState@(GameState pacman food ghosts gen score lives screen paused)
  | paused || screen /= GameScreen = return gameState
  | otherwise = do
      let updatedPacman = movePacman pacmanGameBoard pacman
          updatedGhosts = map (updateGhostTimer deltaTime) ghosts
          releasedGhosts = map (releaseGhostIfNeeded pacmanGameBoard) updatedGhosts
          (movedGhosts, newGen) = moveGhosts gen pacmanGameBoard updatedPacman releasedGhosts
          (foodEaten, newFood) = pacmanEatsFood updatedPacman food
          scoreIncrement = if foodEaten then 1 else 0
          newLives = pacmanLoosesLife updatedPacman movedGhosts lives
          newScore = score + scoreIncrement
          newGameScreen = if newLives == 0 then GameOverScreen else GameScreen

      if newLives < lives
        then if newLives > 0
          then return $ resetPositionsAndLooseLife gameState
                 { score = newScore, food = newFood, screen = newGameScreen, randGen = newGen, lives = newLives }
          else do
            writeHighScore newScore
            return $ restartGame gameState { score = newScore }
        else return gameState
                 { pacman = updatedPacman, ghosts = movedGhosts, food = newFood, score = newScore, screen = newGameScreen, randGen = newGen }
