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
  -- If pacman isFull = True, then draw a full pacman, otherwise draw a half pacman
  if isFull pacman
    then drawFullPacman pacman
    else drawOpenPacman pacman

drawOpenPacman :: Pacman -> Picture
drawOpenPacman pacman = 
    translatePosition (Model.Utils.Functions.position pacman) $
    rotate (directionToAngle $ Model.Pacman.Types.direction pacman) pacmanImage

drawFullPacman :: Pacman -> Picture
drawFullPacman pacman = 
    translatePosition (Model.Utils.Functions.position pacman) $
    Color yellow $ circleSolid 5