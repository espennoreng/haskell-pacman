module Controller where

import Boards
import Graphics.Gloss.Interface.Pure.Game
import Model
import View

handleInput :: Event -> Pacman -> Pacman
handleInput (EventKey (SpecialKey KeyUp) _ _ _) pacman = pacman {direction = Model.Up}
handleInput (EventKey (SpecialKey KeyDown) _ _ _) pacman = pacman {direction = Model.Down}
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) pacman = pacman {direction = Model.Left}
handleInput (EventKey (SpecialKey KeyRight) _ _ _) pacman = pacman {direction = Model.Right}
handleInput _ pacman = pacman -- don't change anything for other events

render :: Pacman -> Picture
render = gameBoardToPicture validGameBoard

-- Delta time represents the elapsed time between two consecutive frames in real-time 
-- applications, ensuring consistent behavior across varying frame rates. 
-- By scaling movements or updates with this value, objects in games or simulations 
-- move consistently regardless of how quickly frames are processed.

update :: Float -> Pacman -> Pacman -- dt = delta time (WE NEED TO EXPLAIN THIS BETTER)
update dt = movePacman dt validGameBoard -- WE NEED TO EXPLAIN THIS BETTER (dt)
