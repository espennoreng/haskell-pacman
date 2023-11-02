module Controller.Update where

import Model.GameState.Types
import Model.GameState.Functions
import Model.Pacman.Functions
import Model.Ghosts.Functions
import Model.Board.GameBoard
import Controller.HighScore


handleUpdate :: Float -> GameState -> IO GameState
handleUpdate deltaTime gameState@(GameState pacman food ghosts gen score lives screen paused) =
  if paused || screen /= GameScreen then
    return gameState
  else do
    let updatedPacman = movePacman pacmanGameBoard pacman
        updatedGhosts = map (updateGhostTimer deltaTime) ghosts
        releasedGhosts = map (releaseGhostIfNeeded pacmanGameBoard) updatedGhosts
        (movedGhosts, newGen) = moveGhosts gen pacmanGameBoard updatedPacman releasedGhosts
        (foodEaten, newFood) = pacmanEatsFood updatedPacman food
        scoreIncrement = if foodEaten then 1 else 0
        newScore = score + scoreIncrement
        newLives = pacmanLoosesLife updatedPacman movedGhosts lives
        newGameScreen = if newLives == 0 then GameOverScreen else GameScreen

    -- Handling score and high score
    if newLives == 0 then
      do writeHighScore newScore
         return $ restartGame gameState { score = newScore }
    else if newLives < lives then
      return $ resetPositionsAndLooseLife gameState { score = newScore, food = newFood, screen = newGameScreen, randGen = newGen, lives = newLives }
    else
      return $ gameState { pacman = updatedPacman, ghosts = movedGhosts, food = newFood, score = newScore, screen = newGameScreen, randGen = newGen }
   