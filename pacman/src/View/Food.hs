module View.Food where

import Graphics.Gloss
import Model.GameState.Types
import Model.Utils.Functions

foodToPicture :: Food -> Picture
foodToPicture food = translatePosition food $ Color cyan $ circleSolid 2


