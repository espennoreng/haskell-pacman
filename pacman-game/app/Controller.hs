module Controller where

import Boards ( pacmanGameBoard )
import Graphics.Gloss.Interface.Pure.Game
    ( Picture,
      Key(SpecialKey),
      Event(EventKey),
      SpecialKey(KeyRight, KeyUp, KeyDown, KeyLeft) )
import Model
    ( movePacman, Direction(Right, Up, Down, Left), Pacman(direction) )
import View ( gameBoardToPicture )

handleInput :: Event -> Pacman -> Pacman
handleInput (EventKey (SpecialKey KeyUp) _ _ _) pacman = pacman {direction = Model.Up}
handleInput (EventKey (SpecialKey KeyDown) _ _ _) pacman = pacman {direction = Model.Down}
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) pacman = pacman {direction = Model.Left}
handleInput (EventKey (SpecialKey KeyRight) _ _ _) pacman = pacman {direction = Model.Right}
handleInput _ pacman = pacman -- don't change anything for other events

render :: Pacman -> Picture
render = gameBoardToPicture pacmanGameBoard


update :: Float -> Pacman -> Pacman
update _ = movePacman pacmanGameBoard