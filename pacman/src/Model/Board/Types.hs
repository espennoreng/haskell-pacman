module Model.Board.Types where
import Model.Utils.Types

data Wall = NormalWall Position | GhostHomeWall Position deriving (Eq, Show)

newtype GameBoard = GameBoard
  {walls :: [Wall]}
  deriving (Eq, Show)

