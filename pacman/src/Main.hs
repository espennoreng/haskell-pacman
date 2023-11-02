module Main where
import qualified Graphics.Gloss.Interface.IO.Game as Game
import Model (initGameState)
import View (background, window)
import Controller (handleInput, render, update)

main :: IO ()
main = do 
    let initialState = initGameState
    Game.playIO window background 5 initialState render handleInput update
