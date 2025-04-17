{
module Lexer (Token (..), lexer) where

import Data.Int (Int32)
import Data.Word (Word32)
import qualified Data.Bits
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$whitespace = [\ \t\r\n]
$separator = [$whitespace\,\;]

tokens :-

  $whitespace+                     ;
  "#".*                            ;
  "/*" (. | \n)* "*/"              ;
  ","                              { \_ -> COMMA }
  $digit+                          { \s -> NUM (read s) }
  "+"                              { \_ -> PLUS }
  "-"                              { \_ -> MINUS } 
  "*"                              { \_ -> TIMES }
  "/"                              { \_ -> DIVIDE }
  "("                              { \_ -> LPAREN }
  ")"                              { \_ -> RPAREN }
  "add"                            { \_ -> ADD }
  "sub"                            { \_ -> SUB }
  "and"                            { \_ -> AND }
  "or"                             { \_ -> OR }
  "xor"                            { \_ -> XOR }
  "slt"                            { \_ -> SLT }
  "sltu"                           { \_ -> SLTU }
  "sll"                            { \_ -> SLL }
  "srl"                            { \_ -> SRL }
  "sra"                            { \_ -> SRA }
  "lb"                             { \_ -> LB }
  "lh"                             { \_ -> LH }
  "lw"                             { \_ -> LW }
  "lbu"                            { \_ -> LBU }
  "lhu"                            { \_ -> LHU }
  "sb"                             { \_ -> SB }
  "sh"                             { \_ -> SH }
  "sw"                             { \_ -> SW }
  "beq"                            { \_ -> BEQ }
  "bne"                            { \_ -> BNE }
  "blt"                            { \_ -> BLT }
  "bge"                            { \_ -> BGE }
  "bltu"                           { \_ -> BLTU }
  "bgeu"                           { \_ -> BGEU }
  "jal"                            { \_ -> JAL }
  "jalr"                           { \_ -> JALR }
  "lui"                            { \_ -> LUI }
  "auipc"                          { \_ -> AUIPC }
  "ecall"                          { \_ -> ECALL }
  "ebreak"                         { \_ -> EBREAK }
  "fence"                          { \_ -> FENCE }
  "fence.i"                        { \_ -> FENCE_I }
  "fence.t"                        { \_ -> FENCE_T }
  "fence.ts"                       { \_ -> FENCE_TS }
  [$alpha $digit]+                 { \s -> REG s }

{
data Token = 
    COMMA
  | NUM Int32
  | PLUS | MINUS | TIMES | DIVIDE
  | LPAREN | RPAREN
  | ADD | SUB | AND | OR | XOR
  | SLT | SLTU | SLL | SRL | SRA
  | LB | LH | LW | LBU | LHU
  | SB | SH | SW
  | BEQ | BNE | BLT | BGE | BLTU | BGEU
  | JAL | JALR
  | LUI | AUIPC
  | ECALL | EBREAK
  | FENCE | FENCE_I | FENCE_T | FENCE_TS
  | REG String
  | EOF
  deriving (Eq, Show)

data LexError = InvalidCharacter String
  deriving (Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
