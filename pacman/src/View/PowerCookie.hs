module View.PowerCookie where

import Graphics.Gloss
import Model.GameState.Types
import Model.Utils.Functions

powerCookieToPicture :: Food -> Picture
powerCookieToPicture food = translatePosition food $ Color cyan $ circleSolid 4
