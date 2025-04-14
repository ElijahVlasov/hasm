{-# LANGUAGE DataKinds #-}

module Test.Immediate (testImmediate) where

import Immediate (Immediate)
import Immediate qualified as Imm
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testImmediate :: TestTree
testImmediate =
  testGroup
    "Test immediates"
    [ testImmediate12Positive
    , testImmediate12Negative
    , testImmediate12OfWord32
    , testImmediate12BackAndForth
    , testImmediate12BackAndForthNegative
    ]

testImmediate12Positive :: TestTree
testImmediate12Positive =
  testCase "positive immediate12 value"
    $ Imm.toInt32 (Imm.fromWord32 0x123 :: Immediate 12)
      @?= 0x123

testImmediate12Negative :: TestTree
testImmediate12Negative =
  testCase "positive immediate12 value"
    $ Imm.toInt32 (Imm.fromWord32 0xfff :: Immediate 12)
      @?= (-1)

testImmediate12OfWord32 :: TestTree
testImmediate12OfWord32 =
  testCase "immediate12 of Word32"
    $ Imm.fromInt32 0x123
      @?= Just (Imm.fromWord32 0x123 :: Immediate 12)

testImmediate12BackAndForth :: TestTree
testImmediate12BackAndForth =
  testCase "immediate12 back and forth"
    $ Just 0x123
      @?= Imm.toInt32
      <$> (Imm.fromInt32 0x123 :: Maybe (Immediate 12))

testImmediate12BackAndForthNegative :: TestTree
testImmediate12BackAndForthNegative =
  testCase "immediate12 back and forth negative"
    $ Just (-56)
      @?= Imm.toInt32
      <$> (Imm.fromInt32 (-56) :: Maybe (Immediate 12))
