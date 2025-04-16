{
module Parser where

import Immediate qualified as Imm
import Instruction (Instruction (..))
import Lexer (Token (..))
import Register qualified 
import Parser.Error (ParsingError (..))
import Utils (okOrLeft)
}

%name parseProgram
%tokentype { Token }
%error { parseError }
%monad { Either ParsingError }

%token
  NUM   { NUM $$ }
  REG   { REG $$ }
  ADD   { ADD }
  SUB   { SUB }
  AND   { AND }
  OR    { OR }
  XOR   { XOR }
  SLT   { SLT }
  SLTU  { SLTU }
  SLL   { SLL }
  SRL   { SRL }
  SRA   { SRA }
  LB    { LB }
  LH    { LH }
  LW    { LW }
  LBU   { LBU }
  LHU   { LHU }
  SB    { SB }
  SH    { SH }
  SW    { SW }
  BEQ   { BEQ }
  BNE   { BNE }
  BLT   { BLT }
  BGE   { BGE }
  BLTU  { BLTU }
  BGEU  { BGEU }
  JAL   { JAL }
  JALR  { JALR }
  LUI   { LUI }
  AUIPC { AUIPC }
  ECALL { ECALL }
  EBREAK { EBREAK }
  FENCE { FENCE }
  FENCE_I { FENCE_I }
  FENCE_T { FENCE_T }
  FENCE_TS { FENCE_TS }
  COMMA { COMMA }
  LPAREN { LPAREN }
  RPAREN { RPAREN }
  EOF   { EOF }

%%

Program : {- empty -}    { [] }
        | Instr Program { $1 : $2 }

Register : REG {% \p -> (okOrLeft (InvalidRegister $1) (Register.fromString $1)) }

Imm12 : NUM {% \p -> (okOrLeft (InvalidImmediate $1) (Imm.fromInt32 src2Imm)) }

Instr : ADD Register COMMA Register COMMA Imm12
      {% \p -> do 
            dst <- $2 p
            src1 <- $4 p
            src2Imm <- $6 p
            return $ Right (src2Imm) }

{
parseError :: [Token] -> Either ParsingError a
parseError _ = Left $ DefaultError "Parse error"
}
