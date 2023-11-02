module Controller.Update where

import Controller.HighScore
import Model.Board.GameBoard
import Model.GameState.Functions
import Model.GameState.Types
import Model.Ghosts.Functions
import Model.Pacman.Functions

handleUpdate :: Float -> GameState -> IO GameState
handleUpdate deltaTime gameState@(GameState pacman food ghosts gen score lives screen paused) =
  if paused || screen /= GameScreen
    then return gameState
    else do
      -- Update the positions and check for collisions.
      let updatedPacman = movePacman pacmanGameBoard pacman
          updatedGhosts = map (updateGhostTimer deltaTime) ghosts
          releasedGhosts = map (releaseGhostIfNeeded pacmanGameBoard) updatedGhosts
          (movedGhosts, newGen) = moveGhosts gen pacmanGameBoard updatedPacman releasedGhosts
          (foodEaten, newFood) = pacmanEatsFood updatedPacman food
          scoreIncrement = if foodEaten then 1 else 0

      -- Calculate the new state after collisions.
      let newLives = pacmanLoosesLife updatedPacman movedGhosts lives
          newScore = score + scoreIncrement
          newGameScreen = if newLives == 0 then GameOverScreen else GameScreen

      -- Check if Pacman died to either reset or continue the game.
      if newLives < lives
        then do
          -- Pacman lost a life, reset positions and update the state.
          if newLives > 0
            then
              return $
                resetPositionsAndLooseLife
                  gameState
                    { score = newScore,
                      food = newFood,
                      screen = newGameScreen,
                      randGen = newGen,
                      lives = newLives
                    }
            else do
              -- Pacman has no more lives, game over.
              writeHighScore newScore
              return $ restartGame gameState {score = newScore}
        else -- No life lost, continue the game with updated state.

          return $
            gameState
              { pacman = updatedPacman,
                ghosts = movedGhosts,
                food = newFood,
                score = newScore,
                screen = newGameScreen,
                randGen = newGen
              }