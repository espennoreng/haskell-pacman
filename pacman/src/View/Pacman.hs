module View.Pacman where

import Graphics.Gloss
import Model.Pacman.Types
import Model.Utils.Functions

import Model.Utils.Types as UtilsTypes
import qualified Model.Pacman.Types as Model.Utils.Functions

import Graphics.Gloss
import Model.Pacman.Types
import Model.Utils.Functions (translatePosition)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (handle, SomeException)

-- | Load a PNG image and handle any exceptions by returning a blank image.
loadPacmanPNG :: FilePath -> Picture
loadPacmanPNG path = unsafePerformIO (handle (\e -> do
    let _ = e :: SomeException
    return Blank) (loadJuicyPNG path >>= maybe (return Blank) return))

-- | Image for Pac-Man.
pacmanImage :: Picture
pacmanImage = loadPacmanPNG "res/pacman.png"

directionToAngle :: UtilsTypes.Direction -> Float
directionToAngle UtilsTypes.Up    = 270    -- 270 degrees for upward
directionToAngle UtilsTypes.Down  = 90     -- 90 degrees for downward
directionToAngle UtilsTypes.Left  = 180    -- 180 degrees for left
directionToAngle UtilsTypes.Right = 0      -- 0 degrees for right

-- Convert Pacman's state to a Picture using the PNG image
pacmanToPicture :: Pacman -> Picture
pacmanToPicture pacman =
  translatePosition (Model.Utils.Functions.position pacman) $
    rotate (directionToAngle $ Model.Pacman.Types.direction pacman) pacmanImage

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