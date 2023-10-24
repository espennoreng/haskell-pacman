import Graphics.Gloss (display)
import Model ()
import Boards (validGameBoard)
import Test (runTests)
import View (background, gameBoardToPicture, window)

main :: IO ()
main = do 
    runTests
    display window background (gameBoardToPicture validGameBoard)