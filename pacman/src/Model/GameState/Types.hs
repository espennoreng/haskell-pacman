module Model.GameState.Types where
import Model.Utils.Types
import Model.Pacman.Types
import Model.Ghosts.Types
import System.Random

type Score = Int

type Lives = Int

type Food = Position

type Paused = Bool

data GameScreen = StartScreen | GameScreen | GameOverScreen deriving (Eq, Show)

data GameState = GameState
  { pacman :: Pacman,
    food :: [Food],
    ghosts :: [Ghost],
    randGen :: StdGen,
    score :: Score,
    lives :: Lives,
    screen :: GameScreen,
    paused :: Paused
  }
  deriving (Eq, Show)
