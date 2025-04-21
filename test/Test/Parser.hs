module Test.Parser (testParser) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Instruction (Instruction (..))
import Parser (lexer, parseProgram)
import Register

testParser :: TestTree
testParser =
  testGroup
    "Test parser"
    [testAddAddAdd]

testAddAddAdd :: TestTree
testAddAddAdd =
  testCase "Test add add add"
    $ let program =
            "\n\
            \  add x1, x2, x3\n\
            \  add x4, x5, x6 # this is a comment    \n\
            \  add x7, \n\
            \           x8\n\
            \           /* this is a multi-line comment \n\
            \           with a newline */\n\
            \           \n\
            \           , \n\
            \           \n\
            \           \n\
            \           x9"
      in  ( parseProgram
              . lexer
          )
            program
            @?= Right
              [ Add x1 x2 (Left x3)
              , Add x4 x5 (Left x6)
              , Add x7 x8 (Left x9)
              ]
