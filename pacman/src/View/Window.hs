module View.Window where

import Graphics.Gloss

window :: Display
window = InWindow "Haskell Pacman" (400, 300) (10, 10)

background :: Color
background = black
