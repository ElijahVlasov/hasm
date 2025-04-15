{-# LANGUAGE DataKinds #-}

module Instruction where

import FenceType (FenceType)
import Immediate (Immediate)
import Instruction.Raw
import Register (Register)

type RegisterOrImm = Either Register (Immediate 12)

data Instruction
  = -- Integer register-register operations
    Add Register Register RegisterOrImm
  | Sub Register Register Register
  | Sll Register Register RegisterOrImm
  | Srl Register Register RegisterOrImm
  | And Register Register RegisterOrImm
  | Xor Register Register RegisterOrImm
  | Or Register Register RegisterOrImm
  | Slt Register Register RegisterOrImm
  | Sltu Register Register RegisterOrImm
  | Sra Register Register RegisterOrImm
  | Lui Register (Immediate 20)
  | Auipc Register (Immediate 20)
  | Lb Register Register (Immediate 12) -- LB rd, offset(rs1)
  | Lh Register Register (Immediate 12) -- LH rd, offset(rs1)
  | Lw Register Register (Immediate 12) -- LW rd, offset(rs1)
  | Lbu Register Register (Immediate 12) -- LBU rd, offset(rs1)
  | Lhu Register Register (Immediate 12) -- LHU rd, offset(rs1)
  | Sb Register Register (Immediate 12) -- SB rs2, offset(rs1)
  | Sh Register Register (Immediate 12) -- SH rs2, offset(rs1)
  | Sw Register Register (Immediate 12) -- SW rs2, offset(rs1)
  | Beq Register Register (Immediate 12)
  | Bne Register Register (Immediate 12)
  | Blt Register Register (Immediate 12)
  | Bltu Register Register (Immediate 12)
  | Bge Register Register (Immediate 12)
  | Bgeu Register Register (Immediate 12)
  | Jal Register (Immediate 21) -- JAL rd, offset
  | Jalr Register Register (Immediate 12) -- JALR rd, rs1, offset
  | Ecall
  | Ebreak
  | Fence FenceType FenceType
  deriving (Eq, Show)

toRaw :: Instruction -> RawInstruction
toRaw (Add dst src1 src2) = add dst src1 src2
toRaw (Sub dst src1 src2) = sub dst src1 src2
toRaw (Sll dst src1 src2) = sll dst src1 src2
toRaw (Srl dst src1 src2) = srl dst src1 src2
toRaw (And dst src1 src2) = Instruction.Raw.and dst src1 src2
toRaw (Xor dst src1 src2) = xor dst src1 src2
toRaw (Or dst src1 src2) = Instruction.Raw.or dst src1 src2
toRaw (Slt dst src1 src2) = slt dst src1 src2
toRaw (Sltu dst src1 src2) = sltu dst src1 src2
toRaw (Sra dst src1 src2) = sra dst src1 src2
toRaw (Lui dst imm) = lui dst imm
toRaw (Auipc dst imm) = auipc dst imm
toRaw (Lb dst base offset) = lb dst offset base
toRaw (Lh dst base offset) = lh dst offset base
toRaw (Lw dst base offset) = lw dst offset base
toRaw (Lbu dst base offset) = lbu dst offset base
toRaw (Lhu dst base offset) = lhu dst offset base
toRaw (Sb src base offset) = sb src offset base
toRaw (Sh src base offset) = sh src offset base
toRaw (Sw src base offset) = sw src offset base
toRaw (Beq src1 src2 offset) = beq src1 src2 offset
toRaw (Bne src1 src2 offset) = bne src1 src2 offset
toRaw (Blt src1 src2 offset) = blt src1 src2 offset
toRaw (Bltu src1 src2 offset) = bltu src1 src2 offset
toRaw (Bge src1 src2 offset) = bge src1 src2 offset
toRaw (Bgeu src1 src2 offset) = bgeu src1 src2 offset
toRaw (Jal dst offset) = jal dst offset
toRaw (Jalr dst base offset) = jalr dst base offset
toRaw Ecall = ecall
toRaw Ebreak = ebreak
toRaw (Fence a b) = fence a b
