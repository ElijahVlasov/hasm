{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty (defaultMain)
import Test.Tasty.Runners (TestTree (TestGroup))

import Test.Immediate (testImmediate)
import Test.Instruction.Raw (testRawInstruction)
import Test.Parser (testParser)
import Test.Parser.SingleInstruction (testSingleInstruction)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  TestGroup
    "Tests"
    [ testImmediate
    , testParser
    , testRawInstruction
    , testSingleInstruction
    ]
