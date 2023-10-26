module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Model 
import View

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) _ _ _) (GameState pacman food ghosts gen score lives) =
    GameState (pacman {direction = Model.Up}) food ghosts gen score lives
handleInput (EventKey (SpecialKey KeyDown) _ _ _) (GameState pacman food ghosts gen score lives) = 
    GameState (pacman {direction = Model.Down}) food ghosts gen score lives
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) (GameState pacman food ghosts gen score lives) = 
    GameState (pacman {direction = Model.Left}) food ghosts gen score lives
handleInput (EventKey (SpecialKey KeyRight) _ _ _) (GameState pacman food ghosts gen score lives) = 
    GameState (pacman {direction = Model.Right}) food ghosts gen score lives
handleInput _ gameState = gameState -- don't change anything for other events

render :: GameState -> Picture
render = gameBoardToPicture pacmanGameBoard

update :: Float -> GameState -> GameState
update _ gameState@(GameState pacman food ghosts gen score lives) =
    let 
        updatedPacman = movePacman pacmanGameBoard pacman
        (updatedGhosts, newGen) = moveGhosts gen pacmanGameBoard pacman ghosts
        (foodEaten, newFood) = pacmanEatsFood updatedPacman food
        scoreIncrement = if foodEaten then 1 else 0
        newScore = score + scoreIncrement
    in gameState { pacman = updatedPacman, ghosts = updatedGhosts, randGen = newGen, score = newScore, food = newFood }
