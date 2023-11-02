module Controller.HighScore where
    
import Control.Monad (when)

readHighScores :: IO [Int]
readHighScores = do
    contents <- readFile "highscores.txt"
    return $ map read $ lines contents

getHighestScore :: IO Int
getHighestScore = do
    maximum <$> readHighScores


writeHighScore :: Int -> IO ()
writeHighScore newScore = do
    highestScore <- getHighestScore
    when (newScore > highestScore) $
        writeFile "highscores.txt" (show newScore)
