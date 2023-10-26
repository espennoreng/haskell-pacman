module Test where

import Model
import Test.HUnit
import Model (getNextMoveBFS)

-- Test if any food is placed inside a wall
testIsPositionFreeValid :: Test
testIsPositionFreeValid =
  TestCase $
    assertEqual
      "Test if valid food is inside wall"
      True
      (isPositionFree pacmanGameBoard (0.1, 0.0))

testIsPositionFreeInvalid :: Test
testIsPositionFreeInvalid =
  TestCase $
    assertEqual
      "Test if food is inside wall"
      False
      (isPositionFree invalidGameBoard (-0.5, -0.5))

testIfAPositionIsFreeOfWallsValid :: Test
testIfAPositionIsFreeOfWallsValid =
  TestCase $
    assertEqual
      "Test if valid position is free of walls"
      True
      (isPositionFreeOfWalls validGameBoard (0.1, 0.0) Model.Right)

testIfAPositionIsFreeOfWallsInvalid :: Test
testIfAPositionIsFreeOfWallsInvalid =
  TestCase $
    assertEqual
      "Test if invalid position is free of walls"
      False
      (isPositionFreeOfWalls validGameBoard (-0.5, -0.5) Model.Right)

-- Test if the board has the correct number of walls.
-- Both boards have 44 walls
testCorrectNumberOfWallsValid :: Test
testCorrectNumberOfWallsValid =
  TestCase $
    assertEqual
      "Correct number of walls for valid board"
      True
      (length (walls invalidGameBoard) == 44)

testInitPacman :: Test
testInitPacman =
  TestCase $
    assertEqual
      "Pacman starts in the center facing Right"
      (Pacman {position = (0.0, 0.0), direction = Model.Right})
      initPacman

testMovePacmanRight :: Test
testMovePacmanRight =
  TestCase $
    let
      pacman = initPacman
      movedPacman = movePacman pacmanGameBoard (pacman {direction = Model.Right})
    in
      assertEqual
        "Pacman moves right"
        (Pacman {position = (0.05, 0.0), direction = Model.Right})
        movedPacman


testMovePacmanLeft :: Test
testMovePacmanLeft =
  TestCase $
    let
      pacman = initPacman
      movedPacman = movePacman pacmanGameBoard (pacman {direction = Model.Left})
    in
      assertEqual
        "Pacman moves left"
        (Pacman {position = (-0.05, 0.0), direction = Model.Left})
        movedPacman


testMovePacmanUp :: Test
testMovePacmanUp =
  TestCase $
    let
      pacman = initPacman
      movedPacman = movePacman pacmanGameBoard (pacman {direction = Model.Up})
    in
      assertEqual
        "Pacman moves up"
        (Pacman {position = (0.0, 0.05), direction = Model.Up})
        movedPacman


testMovePacmanDown :: Test
testMovePacmanDown =
  TestCase $
    let
      pacman = initPacman
      movedPacman = movePacman pacmanGameBoard (pacman {direction = Model.Down})
    in
      assertEqual
        "Pacman moves down"
        (Pacman {position = (0.0, -0.05), direction = Model.Down})
        movedPacman


testPacmanCollisionWithWallLeft :: Test
testPacmanCollisionWithWallLeft =
  TestCase $
    let pacman = Pacman {position = (-0.45, 0.0), direction = Model.Left}
        movedPacman = movePacman pacmanGameBoard pacman
    in
      assertEqual
        "Pacman should not move left into wall"
        pacman
        movedPacman

testPacmanCollisionWithWallRight :: Test
testPacmanCollisionWithWallRight =
  TestCase $
    let pacman = Pacman {position = (0.45, 0.0), direction = Model.Right}
        movedPacman = movePacman pacmanGameBoard pacman
    in
      assertEqual
        "Pacman should not move right into wall"
        pacman
        movedPacman


testPacmanCollisionWithWallUp :: Test
testPacmanCollisionWithWallUp =
  TestCase $
    let pacman = Pacman {position = (0.0, 0.45), direction = Model.Up}
        movedPacman = movePacman pacmanGameBoard pacman
    in
      assertEqual
        "Pacman should not move up into wall"
        pacman
        movedPacman

testPacmanCollisionWithWallDown :: Test
testPacmanCollisionWithWallDown =
  TestCase $
    let pacman = Pacman {position = (0.0, -0.45), direction = Model.Down}
        movedPacman = movePacman pacmanGameBoard pacman
    in
      assertEqual
        "Pacman should not move down into wall"
        pacman
        movedPacman

testIsPositionInBoundsValid :: Test
testIsPositionInBoundsValid =
  TestCase $
    assertEqual
      "Test if valid position is in bounds"
      True
      (isPositionInBounds (0.1, 0.0))

testIsPositionInBoundsInvalid :: Test
testIsPositionInBoundsInvalid =
  TestCase $
    assertEqual
      "Test if invalid position is in bounds"
      False
      (isPositionInBounds (0.6, 0.0))


-- Testing the movement of blinky the ghost

-- Mock Data
sampleBoard :: GameBoard
sampleBoard = testBoard

blinkyAt :: Position -> Ghost
blinkyAt pos = Ghost { ghostPosition = pos, ghostDirection = Model.Right, ghostType = Blinky, ghostMode = Chase }

-- Test Cases
testCases :: [(String, GameBoard, Ghost, Position, Direction)]
testCases = 
    [ ("Test if Blinky chooses Up when target is above", sampleBoard, blinkyAt (5, 5), (5, 7), Up),
      ("Test if Blinky chooses Down when target is below", sampleBoard, blinkyAt (5, 5), (5, 3), Down)    ]

-- Testing Function
testGetNextMove :: (String, GameBoard, Ghost, Position, Direction) -> IO ()
testGetNextMove (desc, board, ghost, target, expectedDir) = do
    let result = getNextMoveBFS board ghost target
    if result == expectedDir
    then putStrLn $ "Passed: " ++ desc
    else putStrLn $ "Failed: " ++ desc ++ " Expected: " ++ show expectedDir ++ " but got: " ++ show result

-- Group tests together
tests :: Test
tests =
  TestList
    [ 
      -- Just run testGetNextMove
      TestLabel "Test getNextMove" $ TestList $ map (TestCase . testGetNextMove) testCases
    ]

-- Function to run all tests
runTests :: IO ()
runTests = runTestTT tests >>= print
