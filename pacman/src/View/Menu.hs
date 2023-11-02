module View.Menu where

import Graphics.Gloss.Interface.Pure.Game

pauseScreenPicture :: Picture
pauseScreenPicture = translate 0 (-120) $ Color white $ Scale 0.1 0.1 $
  Text "Press Space to resume"

notPauseScreenPicture :: Picture
notPauseScreenPicture = translate 0 (-120) $ Color white $ Scale 0.1 0.1 $
  Text "Press Space to pause"


startScreenPicture :: Int -> Picture
startScreenPicture score =
  pictures
    [ Translate (-100) 0 $ Scale 0.2 0.2 $ Color white $ Text "Haskell Pacman",
      Translate (-100) (-100) $ Scale 0.1 0.1 $ Color white $ Text "Press Enter to start",
      Translate (-100) (-120) $ Scale 0.1 0.1 $ Color white $ Text ("High Score: " ++ show score)
    ]

gameOverPicture :: Int -> Int -> Picture
gameOverPicture score highScore =
  pictures
    [ Translate (-100) 0 $ Scale 0.2 0.2 $ Color white $ Text "Game Over",
      Translate (-100) (-100) $ Scale 0.1 0.1 $ Color white $ Text $ "Score: " ++ show score,
      Translate (-100) (-120) $ Scale 0.1 0.1 $ Color white $ Text $ "High Score: " ++ show highScore,
      Translate (-100) (-40) $ Scale 0.1 0.1 $ Color white $ Text "Press Enter to restart"
    ]