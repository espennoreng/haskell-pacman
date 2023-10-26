module Controller where

import Graphics.Gloss.Interface.Pure.Game
    ( Picture,
      Key(SpecialKey),
      Event(EventKey),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft) )
import Model
    ( movePacman, moveGhost, Direction(Right, Up, Down, Left), Pacman(direction), Food, pacmanGameBoard, GameState (GameState, pacman, ghosts) )
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
render = gameBoardToPicture pacmanGameBoard
update :: Float -> GameState -> GameState
update _ gameState@(GameState pacman food ghosts) =
    -- Update Pacman and Ghosts positions
    let updatedPacman = movePacman pacmanGameBoard pacman
        updatedGhosts = map (moveGhost pacmanGameBoard) ghosts
    in gameState { pacman = updatedPacman, ghosts = updatedGhosts }