import Graphics.Gloss (play)
import Model (initGameState)
import Test (runTests)
import View (background, gameBoardToPicture, window)
import Controller (handleInput, render, update)

main :: IO ()
main = do 
    runTests
    let initialState = initGameState
    play window background 5 initialState render handleInput update
