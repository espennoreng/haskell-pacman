module Controller where

import Graphics.Gloss.Interface.Pure.Game
    ( Picture,
      Key(SpecialKey),
      Event(EventKey),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft) )
import Model
    ( movePacman, Direction(Right, Up, Down, Left), Pacman(direction), Food, pacmanGameBoard, testBoard, GameState (GameState, pacman, ghosts, randGen), moveGhosts )
import View ( gameBoardToPicture )

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) _ _ _) (GameState pacman food ghosts gen) =
    GameState (pacman {direction = Model.Up}) food ghosts gen
handleInput (EventKey (SpecialKey KeyDown) _ _ _) (GameState pacman food ghosts gen) = 
    GameState (pacman {direction = Model.Down}) food ghosts gen
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) (GameState pacman food ghosts gen) = 
    GameState (pacman {direction = Model.Left}) food ghosts gen
handleInput (EventKey (SpecialKey KeyRight) _ _ _) (GameState pacman food ghosts gen) = 
    GameState (pacman {direction = Model.Right}) food ghosts gen
handleInput _ gameState = gameState -- don't change anything for other events

render :: GameState -> Picture
render = gameBoardToPicture pacmanGameBoard

update :: Float -> GameState -> GameState
update _ gameState@(GameState pacman food ghosts gen) =
    let updatedPacman = movePacman pacmanGameBoard pacman
        (updatedGhosts, newGen) = moveGhosts gen pacmanGameBoard pacman ghosts
    in gameState { pacman = updatedPacman, ghosts = updatedGhosts, randGen = newGen }
