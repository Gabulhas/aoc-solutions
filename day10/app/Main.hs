module Main where
import Data.List
import Control.Monad

parseInt :: String -> Int
parseInt = read 

doesPrintSignalStrength :: Int -> Int -> IO ()
doesPrintSignalStrength cycle x =
    Control.Monad.when ((cycle - 20 ) `mod` 40 == 0) $ print (x * cycle)

executeProgram :: [String] -> Int -> Int -> IO ()
executeProgram [] cycle x = return () 
executeProgram (cLine:restOfProgram) cycle x = do
  _ <- doesPrintSignalStrength cycle x
  let opcode = words cLine
  if head opcode == "noop"
    then do
      _ <- doesPrintSignalStrength cycle x
      executeProgram restOfProgram (cycle + 1) x
    else do
      let chg = parseInt (last opcode)
      _ <- doesPrintSignalStrength cycle x
      _ <- doesPrintSignalStrength (cycle + 1) x
      executeProgram restOfProgram (cycle + 2) (x + chg)

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input
  print inputLines
  executeProgram inputLines 1 1
