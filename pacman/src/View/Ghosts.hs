module View.Ghosts where

import Graphics.Gloss
import Model.Ghosts.Types
import Model.Utils.Functions
import Model.Utils.Types (Position)

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Model.Ghosts.Types
import Model.Utils.Functions
import Model.Utils.Types (Position)
import Control.Exception (handle, SomeException)
import System.IO.Unsafe (unsafePerformIO)

-- | Load a PNG image and handle any exceptions by returning a blank image.
loadPNG :: FilePath -> Picture
loadPNG path = unsafePerformIO (handle (\e -> do
    let _ = e :: SomeException
    return $ Color magenta $ circleSolid 10) -- Placeholder in case the image fails to load
    (loadJuicyPNG path >>= maybe (return Blank) return))

-- | Images for the ghosts.
blinkyImage :: Picture
blinkyImage = loadPNG "res/blinky.png"

pinkyImage :: Picture
pinkyImage = loadPNG "res/pinky.png"

inkyImage :: Picture
inkyImage = loadPNG "res/inky.png"

clydeImage :: Picture
clydeImage = loadPNG "res/clyde.png"

scaredImage :: Picture
scaredImage = loadPNG "res/scared.png"

-- Get the appropriate ghost image based on the ghost type.
getGhostImage :: GhostType -> Picture
getGhostImage Blinky = blinkyImage
getGhostImage Pinky  = pinkyImage
getGhostImage Inky   = inkyImage
getGhostImage Clyde  = clydeImage

-- Convert a Ghost data structure to a Picture using PNG images.
ghostToPicture :: Ghost -> Picture
ghostToPicture (Ghost kind pos _ _ frightenedTimer) =
  if frightenedTimer > 0
    then Model.Utils.Functions.translatePosition pos scaredImage
  else Model.Utils.Functions.translatePosition pos $ getGhostImage kind

-- This function should be adjusted to properly place the ghost's image in your game's coordinate system.
translatePosition :: Position -> Picture -> Picture
translatePosition (x, y) = translate x y
