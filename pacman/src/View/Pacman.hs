module View.Pacman where

import Graphics.Gloss
import Model.Pacman.Types
import Model.Utils.Functions

pacmanToPicture :: Pacman -> Picture
pacmanToPicture pacman =
  translatePosition (position pacman) $
    Color yellow $
      -- arcSolid takes start and end angles in degrees, 0 degrees is along the x axis and positive is counter clockwise.
      -- Adjust the angles to create a mouth for Pacman
      arcSolid startAngle endAngle pacmanRadius
  where
    pacmanRadius = 5 -- Define the radius of Pacman
    mouthSize = 60     -- Define the size of the mouth opening in degrees
    startAngle = -mouthSize / 2 -- Start angle for the arc
    endAngle = mouthSize / 2    -- End angle for the arc

pacmanIconSize :: Float
pacmanIconSize = 4 -- Size of the pacman icon for lives

-- The horizontal spacing between each icon.
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
    
    -- Text "Lives: "
    livesText = translate textX textY $ scale textScale textScale $ color white $ text "Lives: "
    
    -- Position for the icons
    iconOffsetX = 3 -- Starting offset for the first icon from the text
    
    -- Draw a single life icon
    drawLifeIcon = color yellow $ circleSolid pacmanIconSize
    
    -- Create a picture for all the lives icons
    livesIcons = pictures [translate (textX + iconOffsetX + fromIntegral n * (pacmanIconSize + iconSpacing)) startingY drawLifeIcon | n <- [0..lives]]