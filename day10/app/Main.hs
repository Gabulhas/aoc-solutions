module Main where

import Control.Monad
import Data.List
import Debug.Trace
import Text.Printf

parseInt :: String -> Int
parseInt = read

accumStrength :: Int -> Int -> Int -> Int
accumStrength cycle x currentSum =
  if (cycle - 20) `mod` 40 == 0
    then do
      trace ("cycle: " ++ show cycle ++ ", x: " ++ show x ++ ", str: " ++ show (cycle * x)) $ -- use trace to print the values
        currentSum + (cycle * x)
    else currentSum

executeProgramPartOne :: [String] -> Int -> Int -> Int -> Int
executeProgramPartOne [] cycle x sum_of_strs = sum_of_strs
executeProgramPartOne (cLine : restOfProgram) cycle x sum_of_strs = do
  if cycle > 220
    then sum_of_strs
    else do
      let opcode = words cLine
      if head opcode == "noop"
        then do
          executeProgramPartOne restOfProgram (cycle + 1) x (accumStrength cycle x sum_of_strs)
        else do
          let chg = parseInt (last opcode)
          executeProgramPartOne restOfProgram (cycle + 2) (x + chg) (accumStrength (cycle + 1) x (accumStrength cycle x sum_of_strs))

cycleToPosition :: Int -> Int
cycleToPosition cycle = do
    let pos = cycle `mod` 40
     in case pos of 
          0 -> 39 
          _ -> pos - 1


drawIntoList :: [Bool] -> Int -> Int -> Int -> [Bool]
drawIntoList currentRow



executeProgramPartTwo :: [String] -> Int -> Int -> Int -> [Bool]
executeProgramPartTwo [] cycle spritePos sum_of_strs = print ""
executeProgramPartTwo (cLine : restOfProgram) cycle spritePos sum_of_strs = do
  let currentSpritePositions = [spritePos - 1, spritePos, spritePos + 1]
  let currentDrawingPosition = cycleToPosition cycle

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input
  print (executeProgramPartOne inputLines 1 1 0)
  executeProgramPartTwo
