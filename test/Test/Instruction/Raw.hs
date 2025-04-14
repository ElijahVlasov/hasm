module Test.Instruction.Raw (testRawInstruction) where

import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import FenceType qualified as FT
import Immediate qualified
import Instruction.Raw as Raw
import Register (x0, x1, x2, x3)

testAddX0X0X0 :: TestTree
testAddX0X0X0 =
  testCase "add x0, x0, x0"
    $ add x0 x0 (Left x0) @?= RawInstruction 0x00000033

testAddX1X2X3 :: TestTree
testAddX1X2X3 =
  testCase "add x1, x2, x3"
    $ add x1 x2 (Left x3) @?= RawInstruction 0x003100b3

testAddiX1X2_3 :: TestTree
testAddiX1X2_3 =
  testCase "addi x1, x2, 3"
    $ add x1 x2 (Right (Immediate.fromWord32 3))
      @?= RawInstruction 0x00310093

testAddiX0X0_neg42 :: TestTree
testAddiX0X0_neg42 =
  testCase "addi x0, x0, -42"
    $ add x0 x0 (Right (fromJust $ Immediate.fromInt32 (-42)))
      @?= RawInstruction 0xfd600013

testSubX1X2X3 :: TestTree
testSubX1X2X3 =
  testCase "sub x1, x2, x3"
    $ sub x1 x2 x3 @?= RawInstruction 0x403100b3

testSllX1X2X3 :: TestTree
testSllX1X2X3 =
  testCase "sll x1, x2, x3"
    $ sll x1 x2 (Left x3) @?= RawInstruction 0x003110b3

testSrlX1X2X3 :: TestTree
testSrlX1X2X3 =
  testCase "srl x1, x2, x3"
    $ srl x1 x2 (Left x3) @?= RawInstruction 0x003150b3

testXorX1X2X3 :: TestTree
testXorX1X2X3 =
  testCase "xor x1, x2, x3"
    $ xor x1 x2 (Left x3) @?= RawInstruction 0x003140b3

testOrX1X2X3 :: TestTree
testOrX1X2X3 =
  testCase "or x1, x2, x3"
    $ Raw.or x1 x2 (Left x3) @?= RawInstruction 0x003160b3

testAndX1X2X3 :: TestTree
testAndX1X2X3 =
  testCase "and x1, x2, x3"
    $ Raw.and x1 x2 (Left x3) @?= RawInstruction 0x003170b3

testSltX1X2X3 :: TestTree
testSltX1X2X3 =
  testCase "slt x1, x2, x3"
    $ slt x1 x2 (Left x3) @?= RawInstruction 0x003120b3

testSltuX1X2X3 :: TestTree
testSltuX1X2X3 =
  testCase "sltu x1, x2, x3"
    $ sltu x1 x2 (Left x3) @?= RawInstruction 0x003130b3

testSraX1X2X3 :: TestTree
testSraX1X2X3 =
  testCase "sra x1, x2, x3"
    $ sra x1 x2 (Left x3) @?= RawInstruction 0x403150b3

testLuiX1_1 :: TestTree
testLuiX1_1 =
  testCase "lui x1, 1"
    $ lui x1 (Immediate.fromWord32 1) @?= RawInstruction 0x000010b7

testLuiX0_neg42 :: TestTree
testLuiX0_neg42 =
  testCase "lui x0, -42"
    $ lui x0 (fromJust $ Immediate.fromInt32 (-42))
      @?= RawInstruction 0xfffd6037

testAuipcX1_1 :: TestTree
testAuipcX1_1 =
  testCase "auipc x1, 1"
    $ auipc x1 (Immediate.fromWord32 1) @?= RawInstruction 0x00001097

testAuipcX0_neg42 :: TestTree
testAuipcX0_neg42 =
  testCase "auipc x0, -42"
    $ auipc x0 (fromJust $ Immediate.fromInt32 (-42))
      @?= RawInstruction 0xfffd6017

testLbX1_1_X2 :: TestTree
testLbX1_1_X2 =
  testCase "lb x1, 1(x2)"
    $ lb x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x00110083

testLhX1_1_X2 :: TestTree
testLhX1_1_X2 =
  testCase "lh x1, 1(x2)"
    $ lh x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x00111083

testLwX1_1_X2 :: TestTree
testLwX1_1_X2 =
  testCase "lw x1, 1(x2)"
    $ lw x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x00112083

testLbuX1_1_X2 :: TestTree
testLbuX1_1_X2 =
  testCase "lbu x1, 1(x2)"
    $ lbu x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x00114083

testLhuX1_1_X2 :: TestTree
testLhuX1_1_X2 =
  testCase "lhu x1, 1(x2)"
    $ lhu x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x00115083

testSbX1_1_X2 :: TestTree
testSbX1_1_X2 =
  testCase "sb x1, 1(x2)"
    $ sb x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x001100a3

testShX1_1_X2 :: TestTree
testShX1_1_X2 =
  testCase "sh x1, 1(x2)"
    $ sh x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x001110a3

testSwX1_1_X1 :: TestTree
testSwX1_1_X1 =
  testCase "sw x1, 1(x1)"
    $ sw x1 (Immediate.fromWord32 1) x1 @?= RawInstruction 0x0010a0a3

testSwX1_1_X2 :: TestTree
testSwX1_1_X2 =
  testCase "sw x1, 1(x2)"
    $ sw x1 (Immediate.fromWord32 1) x2 @?= RawInstruction 0x001120a3

testSwX1_222_X2 :: TestTree
testSwX1_222_X2 =
  testCase "sw x1, 222(x2)"
    $ sw x1 (Immediate.fromWord32 222) x2 @?= RawInstruction 0x0c112f23

testBeqX1X2_3 :: TestTree
testBeqX1X2_3 =
  testCase "beq x1, x2, 3"
    $ beq x1 x2 (Immediate.fromWord32 3) @?= RawInstruction 0x00208163

testBneX1X2_3 :: TestTree
testBneX1X2_3 =
  testCase "bne x1, x2, 3"
    $ bne x1 x2 (Immediate.fromWord32 3) @?= RawInstruction 0x00209163

testBltX1X2_3 :: TestTree
testBltX1X2_3 =
  testCase "blt x1, x2, 3"
    $ blt x1 x2 (Immediate.fromWord32 3) @?= RawInstruction 0x0020c163

testBltuX1X2_3 :: TestTree
testBltuX1X2_3 =
  testCase "bltu x1, x2, 3"
    $ bltu x1 x2 (Immediate.fromWord32 3) @?= RawInstruction 0x0020e163

testBgeX1X2_3 :: TestTree
testBgeX1X2_3 =
  testCase "bge x1, x2, 3"
    $ bge x1 x2 (Immediate.fromWord32 3) @?= RawInstruction 0x0020d163

testBgeuX1X2_3 :: TestTree
testBgeuX1X2_3 =
  testCase "bgeu x1, x2, 3"
    $ bgeu x1 x2 (Immediate.fromWord32 3) @?= RawInstruction 0x0020f163

testJalX1_1 :: TestTree
testJalX1_1 =
  testCase "jal x1, 1"
    $ jal x1 (Immediate.fromWord32 1) @?= RawInstruction 0x000000ef

testJalX0_neg42 :: TestTree
testJalX0_neg42 =
  testCase "jal x0, -42"
    $ jal x0 (fromJust $ Immediate.fromInt32 (-42))
      @?= RawInstruction 0xfd7ff06f

testJalX1_2 :: TestTree
testJalX1_2 =
  testCase "jal x1, 2"
    $ jal x1 (Immediate.fromWord32 2) @?= RawInstruction 0x002000ef

testJalrX1X2_1 :: TestTree
testJalrX1X2_1 =
  testCase "jalr x1, 1(x2)"
    $ jalr x1 x2 (Immediate.fromWord32 1) @?= RawInstruction 0x001100e7

testEcall :: TestTree
testEcall =
  testCase "ecall"
    $ ecall @?= RawInstruction 0x00000073

testEbreak :: TestTree
testEbreak =
  testCase "ebreak"
    $ ebreak @?= RawInstruction 0x00100073

testFenceIO :: TestTree
testFenceIO =
  testCase "fence i, o"
    $ fence FT.i FT.o @?= RawInstruction 0x0840000f

testRawInstruction :: TestTree
testRawInstruction =
  testGroup
    "Raw Instruction Tests"
    [ testAddX0X0X0
    , testAddX1X2X3
    , testAddiX1X2_3
    , testAddiX0X0_neg42
    , testSubX1X2X3
    , testSllX1X2X3
    , testSrlX1X2X3
    , testXorX1X2X3
    , testOrX1X2X3
    , testAndX1X2X3
    , testSltX1X2X3
    , testSltuX1X2X3
    , testSraX1X2X3
    , testLuiX1_1
    , testLuiX0_neg42
    , testAuipcX1_1
    , testAuipcX0_neg42
    , testLwX1_1_X2
    , testSwX1_1_X1
    , testSwX1_1_X2
    , testSwX1_222_X2
    , testBeqX1X2_3
    , testBneX1X2_3
    , testBltX1X2_3
    , testBltuX1X2_3
    , testBgeX1X2_3
    , testBgeuX1X2_3
    , testJalX1_1
    , testJalX1_2
    , testJalX0_neg42
    , testJalrX1X2_1
    , testEcall
    , testEbreak
    , testFenceIO
    , testLbX1_1_X2
    , testLhX1_1_X2
    , testLbuX1_1_X2
    , testLhuX1_1_X2
    , testSbX1_1_X2
    , testShX1_1_X2
    ]
