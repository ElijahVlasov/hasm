module Main where

import qualified InstructionBuilder (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  InstructionBuilder.someFunc
