module Main where

import Control.Monad
import Data.List
import Debug.Trace
import Text.Printf

parseInt :: String -> Int
parseInt = read

-- accumStrength :: Int -> Int -> Int -> Int
-- accumStrength cycle x currentSum =
--  if (cycle - 20) `mod` 40 == 0
--    then do
--      trace ("cycle: " ++ show cycle ++ ", x: " ++ show x ++ ", str: " ++ show (cycle * x)) $ -- use trace to print the values
--        currentSum + (cycle * x)
--    else currentSum
--
-- executeProgramPartOne :: [String] -> Int -> Int -> Int -> Int
-- executeProgramPartOne [] cycle x sum_of_strs = sum_of_strs
-- executeProgramPartOne (cLine : restOfProgram) cycle x sum_of_strs = do
--  if cycle > 220
--    then sum_of_strs
--    else do
--      let opcode = words cLine
--      if head opcode == "noop"
--        then do
--          executeProgramPartOne restOfProgram (cycle + 1) x (accumStrength cycle x sum_of_strs)
--        else do
--          let chg = parseInt (last opcode)
--          executeProgramPartOne restOfProgram (cycle + 2) (x + chg) (accumStrength (cycle + 1) x (accumStrength cycle x sum_of_strs))

cycleToPosition :: Int -> Int
cycleToPosition cycle = do
  let pos = cycle `mod` 40
   in case pos of
        0 -> 39
        _ -> pos - 1

tryDrawLine :: Int -> [Int] -> [Bool] -> [Bool]
tryDrawLine cursorPosition [] currentLine = False : currentLine
tryDrawLine cursorPosition (pos : restPositions) currentLine =
  if cursorPosition == pos
    then True : currentLine
    else tryDrawLine cursorPosition restPositions currentLine

tryDrawToScreen :: Int -> Int -> [[Bool]] -> [[Bool]]
tryDrawToScreen cycle pos [] = [[]]
tryDrawToScreen cycle pos (x:xs) = do
  let positions = [pos - 1, pos, pos + 1]
  let cursorPosition = cycleToPosition cycle
  if cursorPosition == 0
    then do
        trace (show (concatMap (\v -> if v then "#" else ".") (reverse x))) $
            tryDrawLine cursorPosition positions [] : xs
    else tryDrawLine cursorPosition positions x : xs

executeProgramPartTwo :: [String] -> Int -> Int -> [[Bool]] -> [[Bool]]
executeProgramPartTwo [] cycle pos result = result
executeProgramPartTwo (cLine : restOfProgram) cycle pos currentScreen = do
  let opcode = words cLine

  if head opcode == "noop"
    then do
      let newScreenState = tryDrawToScreen cycle pos currentScreen
      executeProgramPartTwo restOfProgram (cycle + 1) pos newScreenState
    else do
      let chg = parseInt (last opcode)
      let newScreenState = tryDrawToScreen (cycle + 1) pos (tryDrawToScreen cycle pos currentScreen)
      executeProgramPartTwo restOfProgram (cycle + 2) (chg + pos) newScreenState

printScreen :: [[Bool]] -> IO ()
printScreen [] = print ""
printScreen screen =
  forM_ (reverse screen) (print . concatMap (\v -> if v then "#" else ".") . reverse)

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input
  let screensInverted = executeProgramPartTwo inputLines 1 1 [[]]
  printScreen screensInverted
