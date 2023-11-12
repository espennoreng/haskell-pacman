module View.GameBoard where

import Graphics.Gloss

import View.Food
import View.Ghosts
import View.Pacman
import View.PowerCookie
import View.Walls
import View.Window
import View.Score
import View.Menu
import Model.GameState.Types
import Model.Board.GameBoard
import Model.Board.Types


gameBoardToPicture :: GameBoard -> GameState -> Picture
gameBoardToPicture (GameBoard walls) (GameState pacman foods powerCookies ghosts _ score lives _ paused) =
  pictures $
      [wallToPicture wall | wall <- walls]
      ++ [foodToPicture food | food <- foods]
      ++ [powerCookieToPicture powerCookie | powerCookie <- powerCookies]
      ++ [ghostToPicture ghost | ghost <- ghosts]
      ++ [pacmanToPicture pacman]
      ++ [gameScoreToPicture score]
      ++ [pacmanLivesToPicture lives]
      ++ [if paused then pauseScreenPicture else notPauseScreenPicture]
