module Controller where

import Graphics.Gloss.Interface.Pure.Game
    ( Picture,
      Key(SpecialKey),
      Event(EventKey),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft) )
import Model
    ( movePacman, Direction(Right, Up, Down, Left), Pacman(direction), Food, pacmanGameBoard, testBoard, GameState (GameState, pacman, ghosts), moveGhosts )
import View ( gameBoardToPicture )

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) _ _ _) (GameState pacman food ghosts) =
    GameState (pacman {direction = Model.Up}) food ghosts
handleInput (EventKey (SpecialKey KeyDown) _ _ _) (GameState pacman food ghosts) = 
    GameState (pacman {direction = Model.Down}) food ghosts
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) (GameState pacman food ghosts) = 
    GameState (pacman {direction = Model.Left}) food ghosts
handleInput (EventKey (SpecialKey KeyRight) _ _ _) (GameState pacman food ghosts) = 
    GameState (pacman {direction = Model.Right})food ghosts
handleInput _ gameState = gameState -- don't change anything for other events


render :: GameState -> Picture
render = gameBoardToPicture testBoard

update :: Float -> GameState -> GameState
update _ gameState@(GameState pacman food ghosts) =
    -- Update Pacman and Ghosts positions
    let updatedPacman = movePacman testBoard pacman
        updatedGhosts = moveGhosts testBoard pacman ghosts
    in gameState { pacman = updatedPacman, ghosts = updatedGhosts }