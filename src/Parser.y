{
{-# LANGUAGE DataKinds #-}
module Parser (parseProgram, lexer) where

import Immediate (Immediate (..))
import Immediate qualified as Imm
import Instruction (Instruction (..))
import Lexer (Token (..), lexer)
import Parser.Error (ParsingError (..))
import Register qualified 
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

Register : REG {% 
  okOrLeft 
    (InvalidRegister $1) 
    (Register.fromString $1) }

Imm12 : NUM {% 
  okOrLeft 
    (InvalidImmediate $1) 
    (Imm.fromInt32 $1) }

Imm20 : NUM {% 
  okOrLeft 
    (InvalidImmediate $1) 
    (Imm.fromInt32 $1 :: Maybe (Immediate 20)) }

Imm21 : NUM {% 
  okOrLeft 
    (InvalidImmediate $1) 
    (Imm.fromInt32 $1 :: Maybe (Immediate 21)) }

Instr 
    : ADD Register COMMA Register COMMA Imm12
      { Add $2 $4 (Right $6) }
    | ADD Register COMMA Register COMMA Register 
      { Add $2 $4 (Left $6) }
    | SUB Register COMMA Register COMMA Register
      { Sub $2 $4 $6 }
    | SLL Register COMMA Register COMMA Register
      { Sll $2 $4 (Left $6) }
    | SLL Register COMMA Register COMMA Imm12
      { Sll $2 $4 (Right $6) }
    | SRL Register COMMA Register COMMA Register
      { Srl $2 $4 (Left $6) }
    | SRL Register COMMA Register COMMA Imm12
      { Srl $2 $4 (Right $6) }
    | AND Register COMMA Register COMMA Register
      { And $2 $4 (Left $6) }
    | AND Register COMMA Register COMMA Imm12
      { And $2 $4 (Right $6) }
    | XOR Register COMMA Register COMMA Register
      { Xor $2 $4 (Left $6) }
    | XOR Register COMMA Register COMMA Imm12
      { Xor $2 $4 (Right $6) }
    | OR Register COMMA Register COMMA Register
      { Or $2 $4 (Left $6) }
    | OR Register COMMA Register COMMA Imm12
      { Or $2 $4 (Right $6) }
    | SLT Register COMMA Register COMMA Register
      { Slt $2 $4 (Left $6) }
    | SLT Register COMMA Register COMMA Imm12
      { Slt $2 $4 (Right $6) }
    | SLTU Register COMMA Register COMMA Register
      { Sltu $2 $4 (Left $6) }
    | SLTU Register COMMA Register COMMA Imm12
      { Sltu $2 $4 (Right $6) }
    | SRA Register COMMA Register COMMA Register
      { Sra $2 $4 (Left $6) }
    | SRA Register COMMA Register COMMA Imm12
      { Sra $2 $4 (Right $6) }
    | LUI Register COMMA Imm20
      { Lui $2 $4 }
    | AUIPC Register COMMA Imm20
      { Auipc $2 $4 }
    | LB Register COMMA Imm12 LPAREN Register RPAREN
      { Lb $2 $6 $4 }
    | LH Register COMMA Imm12 LPAREN Register RPAREN
      { Lh $2 $6 $4 }
    | LW Register COMMA Imm12 LPAREN Register RPAREN
      { Lw $2 $6 $4 }
    | LBU Register COMMA Imm12 LPAREN Register RPAREN
      { Lbu $2 $6 $4 }
    | LHU Register COMMA Imm12 LPAREN Register RPAREN
      { Lhu $2 $6 $4 }
    | SB Register COMMA Imm12 LPAREN Register RPAREN
      { Sb $2 $6 $4 }
    | SH Register COMMA Imm12 LPAREN Register RPAREN
      { Sh $2 $6 $4 }
    | SW Register COMMA Imm12 LPAREN Register RPAREN
      { Sw $2 $6 $4 }
    | BEQ Register COMMA Register COMMA Imm12
      { Beq $2 $4 $6 }
    | BNE Register COMMA Register COMMA Imm12
      { Bne $2 $4 $6 }
    | BLT Register COMMA Register COMMA Imm12
      { Blt $2 $4 $6 }
    | BLTU Register COMMA Register COMMA Imm12
      { Bltu $2 $4 $6 }
    | BGE Register COMMA Register COMMA Imm12
      { Bge $2 $4 $6 }
    | BGEU Register COMMA Register COMMA Imm12
      { Bgeu $2 $4 $6 }
    | JAL Register COMMA Imm21
      { Jal $2 $4 }
    | JALR Register COMMA Register COMMA Imm12
      { Jalr $2 $4 $6 }
    | ECALL
      { Ecall }
    | EBREAK
      { Ebreak }

{
parseError :: [Token] -> Either ParsingError a
parseError _ = Left $ DefaultError "Parse error"
}
