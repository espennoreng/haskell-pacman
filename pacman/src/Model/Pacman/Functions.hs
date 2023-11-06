module Model.Pacman.Functions where
import Model.Ghosts.Types
import Model.Pacman.Types
import Model.GameState.Types
import Model.Board.Types
import Model.Utils.Types as UtilsTypes

import Model.Utils.Functions
import Model.Board.Functions

import Data.List (find)


initPacman :: Pacman
initPacman =
  Pacman
    { position = (0, -0.45),
      direction = UtilsTypes.Right,
      lastSuccessfulDirection = UtilsTypes.Right
    }

movePacman :: GameBoard -> Pacman -> Pacman
movePacman = moveEntity pacmanMovable

pacmanMovable :: Movable Pacman
pacmanMovable =
  Movable
    { getPosition = position,
      getDirection = direction,
      setPosition = \newPos p -> p {position = newPos},
      getLastSuccessfulDirection = lastSuccessfulDirection,
      setLastSuccessfulDirection = \dir p -> p {lastSuccessfulDirection = dir}
    }

pacmanEatsFood :: Pacman -> [Food] -> (Bool, [Food])
pacmanEatsFood pacman food =
  let pacmanPos = position pacman
      foodEaten = find (intersects pacmanPos) food
   in case foodEaten of
        Just f -> (True, filter (/= f) food)
        Nothing -> (False, food)

pacmanLoosesLife :: Pacman -> [Ghost] -> Lives -> Lives
pacmanLoosesLife pacman ghosts lives =
  if any (intersects (position pacman) . ghostPosition) ghosts
    then
      if lives > 0
        then lives - 1
        else 0
    else lives

