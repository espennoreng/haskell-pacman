module Controller.Input where

import qualified Graphics.Gloss.Interface.IO.Game as Game
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Game as PureGame
import Model.GameState.Functions
import Model.GameState.Types
import Model.Pacman.Types
import Model.Utils.Types as UtilsTypes
import System.Random

handleInput :: Event -> GameState -> IO GameState
handleInput event gameState@(GameState {screen = currentScreen}) =
  return $ case currentScreen of
    GameScreen -> handleGameScreenInput event gameState
    StartScreen -> handleStartScreenInput event gameState
    GameOverScreen -> handleGameOverScreenInput event gameState

handleGameScreenInput :: Event -> GameState -> GameState
handleGameScreenInput event gameState = case event of
  EventKey (SpecialKey KeyEnter) PureGame.Down _ _ -> gameState {screen = StartScreen}
  EventKey (SpecialKey KeyEsc) PureGame.Down _ _ -> gameState {screen = StartScreen}
  EventKey (SpecialKey KeySpace) PureGame.Down _ _ -> togglePause gameState
  EventKey key PureGame.Down _ _ -> handleMovement key gameState
  _ -> gameState

handleMovement :: Key -> GameState -> GameState
handleMovement key gameState@(GameState {pacman = pacman}) =
  let newDirection = case key of
        (SpecialKey KeyUp) -> UtilsTypes.Up
        (SpecialKey KeyDown) -> UtilsTypes.Down
        (SpecialKey KeyLeft) -> UtilsTypes.Left
        (SpecialKey KeyRight) -> UtilsTypes.Right
        _ -> direction pacman
   in gameState {pacman = pacman {direction = newDirection}}

togglePause :: GameState -> GameState
togglePause gameState = gameState {paused = not (paused gameState)}

handleStartScreenInput :: Event -> GameState -> GameState
handleStartScreenInput event gameState =
  case event of
    EventKey (SpecialKey KeyEnter) PureGame.Down _ _ -> gameState {screen = GameScreen}
    _ -> gameState

handleGameOverScreenInput :: Event -> GameState -> GameState
handleGameOverScreenInput event gameState =
  case event of
    EventKey (SpecialKey KeyEnter) PureGame.Down _ _ ->
      gameState
        { screen = StartScreen,
          randGen = mkStdGen 0
        }
    _ -> gameState
