module Main where

import InstructionBuilder qualified (someFunc)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    InstructionBuilder.someFunc
