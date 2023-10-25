module Controller where

import Graphics.Gloss.Interface.Pure.Game
    ( Picture,
      Key(SpecialKey),
      Event(EventKey),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft) )
import Model
    ( movePacman, Direction(Right, Up, Down, Left), Pacman(direction), Food, pacmanGameBoard, GameState (GameState) )
import View ( gameBoardToPicture )

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) _ _ _) (GameState pacman food) = 
    GameState (pacman {direction = Model.Up}) food
handleInput (EventKey (SpecialKey KeyDown) _ _ _) (GameState pacman food) = 
    GameState (pacman {direction = Model.Down}) food
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) (GameState pacman food) = 
    GameState (pacman {direction = Model.Left}) food
handleInput (EventKey (SpecialKey KeyRight) _ _ _) (GameState pacman food) = 
    GameState (pacman {direction = Model.Right}) food
handleInput _ gameState = gameState -- don't change anything for other events


render :: GameState -> Picture
render = gameBoardToPicture pacmanGameBoard

update :: Float -> GameState -> GameState
update _ (GameState pacman food) = GameState (movePacman pacmanGameBoard pacman) food