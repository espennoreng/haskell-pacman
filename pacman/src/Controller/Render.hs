module Controller.Render where

import Graphics.Gloss.Interface.Pure.Game
import Model.GameState.Types
import Model.Board.GameBoard
import Controller.HighScore
import View.Menu
import View.GameBoard


handleRender :: GameState -> IO Picture
handleRender gameState = case screen gameState of
    StartScreen    -> renderStartScreen gameState
    GameOverScreen -> renderGameOverScreen gameState
    _              -> renderGameScreen gameState

renderStartScreen :: GameState -> IO Picture
renderStartScreen _ = startScreenPicture <$> getHighestScore

renderGameOverScreen :: GameState -> IO Picture
renderGameOverScreen gameState = gameOverPicture (score gameState) <$> getHighestScore

renderGameScreen :: GameState -> IO Picture
renderGameScreen gameState = return $ gameBoardToPicture pacmanGameBoard gameState
