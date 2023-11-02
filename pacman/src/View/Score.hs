module View.Score where

import Graphics.Gloss.Interface.Pure.Game

gameScoreToPicture :: Int -> Picture
gameScoreToPicture score = translate (-100) 110 $ Color white $ Scale 0.1 0.1 $ Text $ "Score: " ++ show score
