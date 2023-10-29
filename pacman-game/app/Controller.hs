module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Model
import View

handleInput :: Event -> GameState -> GameState
handleInput event gameState@(GameState pacman food ghosts gen score lives screen) =
  case screen of
    GameScreen ->
      case event of
        (EventKey (SpecialKey KeyUp) _ _ _) -> gameState {pacman = pacman {direction = Model.Up}}
        (EventKey (SpecialKey KeyDown) _ _ _) -> gameState {pacman = pacman {direction = Model.Down}}
        (EventKey (SpecialKey KeyLeft) _ _ _) -> gameState {pacman = pacman {direction = Model.Left}}
        (EventKey (SpecialKey KeyRight) _ _ _) -> gameState {pacman = pacman {direction = Model.Right}}
        _ -> gameState
    StartScreen ->
      case event of
        (EventKey (SpecialKey KeyEnter) _ _ _) -> gameState {screen = GameScreen}
        _ -> gameState
    GameOverScreen ->
      case event of
        (EventKey (SpecialKey KeyEnter) _ _ _) -> 
            initGameState {randGen = gen, screen = StartScreen}
        _ -> gameState

render :: GameState -> Picture
render gameState =
  case screen gameState of
    GameScreen -> gameBoardToPicture pacmanGameBoard gameState
    StartScreen -> startScreenPicture
    GameOverScreen -> gameOverPicture (score gameState)

update :: Float -> GameState -> GameState
update _ gameState@(GameState pacman food ghosts gen score lives screen) =
  case screen of
    GameScreen ->
      let updatedPacman = movePacman pacmanGameBoard pacman
          (updatedGhosts, newGen) = moveGhosts gen pacmanGameBoard pacman ghosts
          (foodEaten, newFood) = pacmanEatsFood updatedPacman food
          scoreIncrement = if foodEaten then 1 else 0
          newScore = score + scoreIncrement
          newLives = pacmanLoosesLife updatedPacman ghosts lives
          newGameScreen = if newLives == 0 then GameOverScreen else GameScreen
          (finalPacman, finalGhosts) =
            if newLives /= lives
              then
                if newLives == 0
                  then (updatedPacman, []) -- Game over
                  else (initPacman, initGhosts) -- Restart the game
              else (updatedPacman, updatedGhosts) -- Keep the updated positions
       in gameState
            { pacman = finalPacman,
              ghosts = finalGhosts,
              randGen = newGen,
              score = newScore,
              food = newFood,
              lives = newLives,
              screen = newGameScreen
            }
    _ -> gameState -- No change for other screens
