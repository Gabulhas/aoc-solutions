module Main where

import Control.Monad
import Data.List
import Text.Printf
import Debug.Trace

parseInt :: String -> Int
parseInt = read

accumStrength :: Int -> Int -> Int -> Int
accumStrength cycle x currentSum =
  if (cycle - 20) `mod` 40 == 0
    then do
    trace ("cycle: " ++ show cycle ++ ", x: " ++ show x ++ ", str: " ++ show (cycle * x)) $  -- use trace to print the values
        currentSum + (cycle * x)
    else currentSum

executeProgram :: [String] -> Int -> Int -> Int -> Int
executeProgram [] cycle x sum_of_strs = sum_of_strs
executeProgram (cLine : restOfProgram) cycle x sum_of_strs = do
  if cycle > 220 then
    sum_of_strs
  else do
      let opcode = words cLine
      if head opcode == "noop"
        then do
          executeProgram restOfProgram (cycle + 1) x (accumStrength cycle x sum_of_strs)
        else do
          let chg = parseInt (last opcode)
          executeProgram restOfProgram (cycle + 2) (x + chg) (accumStrength (cycle + 1) x (accumStrength cycle x sum_of_strs))

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input
  print (executeProgram inputLines 1 1 0)
