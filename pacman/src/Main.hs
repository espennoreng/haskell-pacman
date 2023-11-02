module Main where
import qualified Graphics.Gloss.Interface.IO.Game as Game
import View.Window (window, background)

import Model.GameState.Functions (initGameState)
import Controller.Input (handleInput)
import Controller.Render (handleRender)
import Controller.Update (handleUpdate)

main :: IO ()
main = do 
    let initialState = initGameState
    Game.playIO window background 5 initialState handleRender handleInput handleUpdate
