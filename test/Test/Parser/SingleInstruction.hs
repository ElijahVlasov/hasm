module Test.Parser.SingleInstruction (testSingleInstruction) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Immediate qualified
import Instruction (Instruction (..))
import Parser (lexer, parseProgram)
import Parser.Error (ParsingError (..))
import Register

testSingleInstruction :: TestTree
testSingleInstruction =
  testGroup
    "Test single instruction parsing"
    [ testEmptyProgram
    , testAddImm
    , testAddReg
    , testWrongLexeme
    , testInvalidSyntax
    , testSubtract
    , testAnd
    , testOr
    , testXor
    , testSlt
    , testSltu
    , testAndi
    , testOri
    , testXori
    , testSlti
    , testSltui
    , testInvalidImmediate
    , testBeq
    , testBne
    , testBlt
    , testBge
    , testBltu
    , testBgeu
    , testLb
    , testLh
    , testLw
    , testLbu
    , testLhu
    , testSb
    , testSh
    , testSw
    , testJal
    , testJalr
    , testLui
    , testAuipc
    , testEcall
    , testEbreak
    ]

testEmptyProgram :: TestTree
testEmptyProgram =
  testCase "empty program parsing"
    $ parseProgram []
      @?= Right []

testAddImm :: TestTree
testAddImm =
  testCase "add x2, x1, 5"
    $ (parseProgram . lexer) "add x2, x1, 5"
      @?= Right [Add x2 x1 (Right $ Immediate.fromWord32 5)]

testAddReg :: TestTree
testAddReg =
  testCase "add x2, x1, x3"
    $ (parseProgram . lexer) "add x2, x1, x3"
      @?= Right [Add x2 x1 (Left x3)]

testWrongLexeme :: TestTree
testWrongLexeme =
  testCase "Wrong lexeme error message"
    $ (parseProgram . lexer) "add y3, x1, x2"
      @?= Left (InvalidRegister "y3")

testInvalidSyntax :: TestTree
testInvalidSyntax =
  testCase "Invalid syntax error message"
    $ (parseProgram . lexer) "add x1, x2"
      @?= Left (DefaultError "Parse error")

testSubtract :: TestTree
testSubtract =
  testCase "sub x1, x2, x3"
    $ (parseProgram . lexer) "sub x1, x2, x3"
      @?= Right [Sub x1 x2 x3]

testAnd :: TestTree
testAnd =
  testCase "and x1, x2, x3"
    $ (parseProgram . lexer) "and x1, x2, x3"
      @?= Right [And x1 x2 (Left x3)]

testOr :: TestTree
testOr =
  testCase "or x1, x2, x3"
    $ (parseProgram . lexer) "or x1, x2, x3"
      @?= Right [Or x1 x2 (Left x3)]

testXor :: TestTree
testXor =
  testCase "xor x1, x2, x3"
    $ (parseProgram . lexer) "xor x1, x2, x3"
      @?= Right [Xor x1 x2 (Left x3)]

testSlt :: TestTree
testSlt =
  testCase "slt x1, x2, x3"
    $ (parseProgram . lexer) "slt x1, x2, x3"
      @?= Right [Slt x1 x2 (Left x3)]

testSltu :: TestTree
testSltu =
  testCase "sltu x1, x2, x3"
    $ (parseProgram . lexer) "sltu x1, x2, x3"
      @?= Right [Sltu x1 x2 (Left x3)]

testAndi :: TestTree
testAndi =
  testCase "and immediate"
    $ (parseProgram . lexer) "and x3, x2, 42"
      @?= Right [And x3 x2 (Right $ Immediate.fromWord32 42)]

testOri :: TestTree
testOri =
  testCase "or immediate"
    $ (parseProgram . lexer) "or x3, x2, 42"
      @?= Right [Or x3 x2 (Right $ Immediate.fromWord32 42)]

testXori :: TestTree
testXori =
  testCase "xor immediate"
    $ (parseProgram . lexer) "xor x3, x2, 42"
      @?= Right [Xor x3 x2 (Right $ Immediate.fromWord32 42)]

testSlti :: TestTree
testSlti =
  testCase "slt immediate"
    $ (parseProgram . lexer) "slt x3, x2, 42"
      @?= Right [Slt x3 x2 (Right $ Immediate.fromWord32 42)]

testSltui :: TestTree
testSltui =
  testCase "sltu immediate"
    $ (parseProgram . lexer) "sltu x3, x2, 42"
      @?= Right [Sltu x3 x2 (Right $ Immediate.fromWord32 42)]

testInvalidImmediate :: TestTree
testInvalidImmediate =
  testCase "Invalid immediate"
    $ (parseProgram . lexer) "add x3, x2, 214748364"
      @?= Left (InvalidImmediate 214748364)

testBeq :: TestTree
testBeq =
  testCase "beq x2, x1, 42"
    $ (parseProgram . lexer) "beq x2, x1, 42"
      @?= Right [Beq x2 x1 (Immediate.fromWord32 42)]

testBne :: TestTree
testBne =
  testCase "bne x2, x1, 42"
    $ (parseProgram . lexer) "bne x2, x1, 42"
      @?= Right [Bne x2 x1 (Immediate.fromWord32 42)]

testBlt :: TestTree
testBlt =
  testCase "blt x2, x1, 42"
    $ (parseProgram . lexer) "blt x2, x1, 42"
      @?= Right [Blt x2 x1 (Immediate.fromWord32 42)]

testBge :: TestTree
testBge =
  testCase "bge x2, x1, 42"
    $ (parseProgram . lexer) "bge x2, x1, 42"
      @?= Right [Bge x2 x1 (Immediate.fromWord32 42)]

testBltu :: TestTree
testBltu =
  testCase "bltu x2, x1, 42"
    $ (parseProgram . lexer) "bltu x2, x1, 42"
      @?= Right [Bltu x2 x1 (Immediate.fromWord32 42)]

testBgeu :: TestTree
testBgeu =
  testCase "bgeu x2, x1, 42"
    $ (parseProgram . lexer) "bgeu x2, x1, 42"
      @?= Right [Bgeu x2 x1 (Immediate.fromWord32 42)]

testLb :: TestTree
testLb =
  testCase "lb x2, 42(x1)"
    $ (parseProgram . lexer) "lb x2, 42(x1)"
      @?= Right [Lb x2 x1 (Immediate.fromWord32 42)]

testLh :: TestTree
testLh =
  testCase "lh x2, 42(x1)"
    $ (parseProgram . lexer) "lh x2, 42(x1)"
      @?= Right [Lh x2 x1 (Immediate.fromWord32 42)]

testLw :: TestTree
testLw =
  testCase "lw x2, 42(x1)"
    $ (parseProgram . lexer) "lw x2, 42(x1)"
      @?= Right [Lw x2 x1 (Immediate.fromWord32 42)]

testLbu :: TestTree
testLbu =
  testCase "lbu x2, 42(x1)"
    $ (parseProgram . lexer) "lbu x2, 42(x1)"
      @?= Right [Lbu x2 x1 (Immediate.fromWord32 42)]

testLhu :: TestTree
testLhu =
  testCase "lhu x2, 42(x1)"
    $ (parseProgram . lexer) "lhu x2, 42(x1)"
      @?= Right [Lhu x2 x1 (Immediate.fromWord32 42)]

testSb :: TestTree
testSb =
  testCase "sb x2, 42(x1)"
    $ (parseProgram . lexer) "sb x2, 42(x1)"
      @?= Right [Sb x2 x1 (Immediate.fromWord32 42)]

testSh :: TestTree
testSh =
  testCase "sh x2, 42(x1)"
    $ (parseProgram . lexer) "sh x2, 42(x1)"
      @?= Right [Sh x2 x1 (Immediate.fromWord32 42)]

testSw :: TestTree
testSw =
  testCase "sw x2, 42(x1)"
    $ (parseProgram . lexer) "sw x2, 42(x1)"
      @?= Right [Sw x2 x1 (Immediate.fromWord32 42)]

testJal :: TestTree
testJal =
  testCase "jal x1, 42"
    $ (parseProgram . lexer) "jal x1, 42"
      @?= Right [Jal x1 (Immediate.fromWord32 42)]

testJalr :: TestTree
testJalr =
  testCase "jalr x1, x2, 42"
    $ (parseProgram . lexer) "jalr x1, x2, 42"
      @?= Right [Jalr x1 x2 (Immediate.fromWord32 42)]

testLui :: TestTree
testLui =
  testCase "lui x1, 42"
    $ (parseProgram . lexer) "lui x1, 42"
      @?= Right [Lui x1 (Immediate.fromWord32 42)]

testAuipc :: TestTree
testAuipc =
  testCase "auipc x1, 42"
    $ (parseProgram . lexer) "auipc x1, 42"
      @?= Right [Auipc x1 (Immediate.fromWord32 42)]

testEcall :: TestTree
testEcall =
  testCase "ecall"
    $ (parseProgram . lexer) "ecall"
      @?= Right [Ecall]

testEbreak :: TestTree
testEbreak =
  testCase "ebreak"
    $ (parseProgram . lexer) "ebreak"
      @?= Right [Ebreak]
