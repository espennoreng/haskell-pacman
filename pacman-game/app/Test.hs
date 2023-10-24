module Test where
import Boards
import Model (Food, GameBoard (..), Wall, isPositionFree, isPositionInBounds)
import Test.HUnit

-- Test if any food is placed inside a wall
testIsPositionFreeValid :: Test
testIsPositionFreeValid =
  TestCase $
    assertEqual
      "Test if valid food is inside wall"
      True
      (isPositionFree validGameBoard (0.1, 0.0))

testIsPositionFreeInvalid :: Test
testIsPositionFreeInvalid =
  TestCase $
    assertEqual
      "Test if food is inside wall"
      False
      (isPositionFree invalidGameBoard (-0.5, -0.5))

-- Test if the board has the correct number of walls.
-- Both boards have 44 walls
testCorrectNumberOfWallsValid :: Test
testCorrectNumberOfWallsValid =
  TestCase $
    assertEqual
      "Correct number of walls for valid board"
      True
      (length (walls validGameBoard) == 44)

-- Group tests together
tests :: Test
tests = TestList [
    testIsPositionFreeValid, 
    testIsPositionFreeInvalid, 
    testCorrectNumberOfWallsValid
    ]

-- Function to run all tests
runTests :: IO ()
runTests = runTestTT tests >>= print
