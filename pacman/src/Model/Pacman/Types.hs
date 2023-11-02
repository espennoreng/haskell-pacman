module Model.Pacman.Types where
import Model.Utils.Types


data Movable a = Movable
  { getPosition :: a -> Position,
    getDirection :: a -> Direction,
    setPosition :: Position -> a -> a,
    getLastSuccessfulDirection :: a -> Direction,
    setLastSuccessfulDirection :: Direction -> a -> a
  }

data Pacman = Pacman
  { position :: Position,
    direction :: Direction,
    lastSuccessfulDirection :: Direction
  }
  deriving (Eq, Show)
