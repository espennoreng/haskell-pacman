module Controller where

import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Interface.IO.Game as Game
import Model
import View

readHighScores :: IO [Int]
readHighScores = do
    contents <- readFile "highscores.txt"
    return $ map read $ lines contents

getHighestScore :: IO Int
getHighestScore = do
    maximum <$> readHighScores

writeHighScore :: Int -> IO ()
writeHighScore newScore = 
    getHighestScore >>= \highestScore ->
        if newScore > highestScore
        then writeFile "highscores.txt" (show newScore)
        else return ()

handleInput :: Event -> GameState -> IO GameState
handleInput event gameState@(GameState pacman food ghosts gen score lives screen paused) =
    return $ -- Wrap the result in an IO action using `return`
        case screen of
            GameScreen ->
                case event of
                    (EventKey (SpecialKey KeyUp) _ _ _) -> gameState {pacman = pacman {direction = Model.Up}}
                    (EventKey (SpecialKey KeyDown) _ _ _) -> gameState {pacman = pacman {direction = Model.Down}}
                    (EventKey (SpecialKey KeyLeft) _ _ _) -> gameState {pacman = pacman {direction = Model.Left}}
                    (EventKey (SpecialKey KeyRight) _ _ _) -> gameState {pacman = pacman {direction = Model.Right}}
                    (EventKey (SpecialKey KeySpace) Game.Down _ _) -> gameState {paused = not paused}
                    _ -> gameState
            StartScreen ->
                case event of
                    (EventKey (SpecialKey KeyEnter) Game.Down _ _) -> gameState {screen = GameScreen}
                    _ -> gameState
            GameOverScreen ->
                case event of
                    (EventKey (SpecialKey KeyEnter) Game.Down _ _) ->
                        gameState {randGen = gen, screen = StartScreen}
                    _ -> gameState


render :: GameState -> IO Picture
render gameState =
  case screen gameState of
    StartScreen -> do
        startScreenPicture <$> getHighestScore
    GameOverScreen -> do
        gameOverPicture (score gameState) <$> getHighestScore
    _ -> return $ gameBoardToPicture pacmanGameBoard gameState

update :: Float -> GameState -> IO GameState
update deltaTime gameState@(GameState pacman food ghosts gen score lives screen paused) =
  if paused || screen /= GameScreen then
    return gameState
  else do
    let updatedPacman = movePacman pacmanGameBoard pacman
        updatedGhosts = map (updateGhostTimer deltaTime) ghosts
        releasedGhosts = map (releaseGhostIfNeeded pacmanGameBoard) updatedGhosts
        (movedGhosts, newGen) = moveGhosts gen pacmanGameBoard updatedPacman releasedGhosts
        (foodEaten, newFood) = pacmanEatsFood updatedPacman food
        scoreIncrement = if foodEaten then 1 else 0
        newScore = score + scoreIncrement
        newLives = pacmanLoosesLife updatedPacman movedGhosts lives
        newGameScreen = if newLives == 0 then GameOverScreen else GameScreen

    -- Handling score and high score
    if newLives == 0 then
      do writeHighScore newScore
         return $ restartGame gameState { score = newScore }
    else if newLives < lives then -- Indicates Pacman lost a life
      return $ resetPositionsAndLooseLife gameState { score = newScore, food = newFood, screen = newGameScreen, randGen = newGen, lives = newLives }
    else
      return $ gameState { pacman = updatedPacman, ghosts = movedGhosts, food = newFood, score = newScore, screen = newGameScreen, randGen = newGen }
   