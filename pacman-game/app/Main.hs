import Graphics.Gloss (play)
import Model (initPacman)
import Boards (validGameBoard)
import Test (runTests)
import View (background, gameBoardToPicture, window)
import Controller (handleInput, render, update)

main :: IO ()
main = do 
    runTests
    let initialState = initPacman
    play window background 10 initialState render handleInput update
