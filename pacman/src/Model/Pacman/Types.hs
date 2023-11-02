module Model.Pacman.Types where
import Model.Utils.Types

data Pacman = Pacman
  { position :: Position,
    direction :: Direction,
    lastSuccessfulDirection :: Direction
  }
  deriving (Eq, Show)
