module Controller.Update where

import Controller.HighScore
import Model.Board.GameBoard
import Model.GameState.Functions
import Model.GameState.Types
import Model.Ghosts.Functions
import Model.Pacman.Functions
import Model.Pacman.Types (Pacman(isFull))

handleUpdate :: Float -> GameState -> IO GameState
handleUpdate deltaTime gameState@(GameState pacman food powerCookies ghosts gen score lives screen paused)
  | paused || screen /= GameScreen = return gameState
  | otherwise = do
      let newIsFull = not (isFull pacman)
          updatedPacman = movePacman pacmanGameBoard pacman
          updatedGhosts = map (updateGhostTimer deltaTime) ghosts
          releasedGhosts = map (releaseGhostIfNeeded pacmanGameBoard) updatedGhosts
          (movedGhosts, newGen) = moveGhosts gen pacmanGameBoard updatedPacman releasedGhosts
          (foodEaten, newFood) = pacmanEatsFood updatedPacman food
          (powerCookieEaten, newPowerCookies) = pacmanEatsPowerCookie updatedPacman powerCookies
          scoreIncrement = if foodEaten then 1 else 0
          updatedGhostsFrigthened = if powerCookieEaten then setFrightenedTimer movedGhosts else movedGhosts
          updatedGhostsDecrementTimer = map (decrementFrightenedTimer deltaTime) updatedGhostsFrigthened
          (pacmanEatsGhost', updatedGhosts') = pacmanEatsGhost updatedPacman updatedGhostsDecrementTimer
          newLives = if pacmanEatsGhost' then lives else pacmanLoosesLife updatedPacman updatedGhosts' lives
          newScore = score + scoreIncrement
          newGameScreen = if newLives == 0 then GameOverScreen else GameScreen
          reSpawnedGhosts = ensureAllGhostsPresent updatedGhosts'
          updatedPacman' = updatedPacman {isFull = newIsFull}

      if newLives < lives
        then
          if newLives > 0
            then
              return $
                resetPositionsAndLooseLife
                  gameState
                    { 
                      pacman = updatedPacman',
                      score = newScore,
                      food = newFood,
                      ghosts = reSpawnedGhosts,
                      powerCookies = newPowerCookies,
                      screen = newGameScreen,
                      randGen = newGen,
                      lives = newLives
                    }
            else do
              writeHighScore newScore
              return $ restartGame gameState {score = newScore}
        else
          return
            gameState
              { pacman = updatedPacman',
                ghosts = reSpawnedGhosts,
                food = newFood,
                powerCookies = newPowerCookies,
                score = newScore,
                screen = newGameScreen,
                randGen = newGen
              }
