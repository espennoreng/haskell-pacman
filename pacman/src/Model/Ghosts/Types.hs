module Model.Ghosts.Types where
import Model.Utils.Types

data GhostType = Blinky | Pinky | Inky | Clyde deriving (Eq, Show)

data GhostMode = Chase | Scatter | Frightened deriving (Eq, Show)

data Ghost = Ghost
  { ghostType :: GhostType,
    ghostPosition :: Position,
    ghostMode :: GhostMode,
    releaseTimer :: Float
  }
  deriving (Eq, Show)