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



pacmanIconSize :: Float
pacmanIconSize = 4

iconSpacing :: Float
iconSpacing = 20

startingX :: Float
startingX = -100 -- Starting X position for the first life
startingY :: Float
startingY = -120 -- Starting Y position for the lives

-- Function to draw a single life as a Pacman icon.
drawLifeIcon :: Picture
drawLifeIcon = Color yellow $ circleSolid pacmanIconSize

-- Function to draw all lives.
pacmanLivesToPicture :: Int -> Picture
pacmanLivesToPicture lives = pictures [livesText, livesIcons]
  where
    -- Position and scale for the text
    textScale = 0.1
    textX = startingX - 70 -- Adjust X position to be before the icons
    textY = startingY + 10 -- Adjust Y position to align with icons if needed
    livesText = translate textX textY $ scale textScale textScale $ color white $ text "Lives: "
    iconOffsetX = 3 -- Starting offset for the first icon from the text
    drawLifeIcon = color yellow $ circleSolid pacmanIconSize
    livesIcons = pictures [translate (textX + iconOffsetX + fromIntegral n * (pacmanIconSize + iconSpacing)) startingY drawLifeIcon | n <- [0..lives]]