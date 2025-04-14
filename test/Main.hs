{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty (defaultMain)
import Test.Tasty.Runners (TestTree (TestGroup))

import Test.Immediate (testImmediate)
import Test.Instruction.Raw (testRawInstruction)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  TestGroup
    "Tests"
    [ testImmediate
    , testRawInstruction
    ]
