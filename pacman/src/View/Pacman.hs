module View.Pacman where

import Graphics.Gloss
import Model.Pacman.Types
import Model.Utils.Functions

import Model.Utils.Types as UtilsTypes
import qualified Model.Pacman.Types as Model.Utils.Functions

directionToAngle :: UtilsTypes.Direction -> Float
directionToAngle UtilsTypes.Up    = 270     -- 90 degrees for upward
directionToAngle UtilsTypes.Down  = 90    -- 270 degrees for downward
directionToAngle UtilsTypes.Left  = 180    -- 180 degrees for left
directionToAngle UtilsTypes.Right = 0      -- 0 degrees for right

-- | Convert Pacman's state to a Picture with a wedge to represent an open mouth
pacmanToPicture :: Pacman -> Picture
pacmanToPicture pacman =
  translatePosition (Model.Utils.Functions.position pacman) $
    rotate (directionToAngle $ Model.Pacman.Types.direction pacman) $
      Color yellow $
        arcSolid (startAngle - mouthSize / 2) (startAngle + mouthSize / 2) pacmanRadius
  where
    pacmanRadius = 5      -- Define the radius of Pacman
    mouthSize = 60         -- Define the size of the mouth opening in degrees
    startAngle = 0         -- 0 degrees is to the right, the default facing direction

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