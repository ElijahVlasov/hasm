{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module InstructionBuilder () where

import Data.Bits (shiftR)

import Immediate (Immediate)
import Immediate qualified
import InstructionBuilder.InstructionChunk
    ( InstructionChunk
    , fromWord32
    )
import Opcode (Opcode)
import Opcode qualified
import Opcode.Funct3 (Funct3)
import Opcode.Funct3 qualified as Funct3
import Opcode.Funct7 (Funct7)
import Opcode.Funct7 qualified as Funct7
import Register (Register)
import Register qualified

opcode
    :: Opcode -> InstructionChunk 0 7
opcode = fromWord32 . Opcode.toWord32

rd
    :: Register -> InstructionChunk 7 12
rd = fromWord32 . Register.toWord32

funct3
    :: Funct3 -> InstructionChunk 12 15
funct3 = fromWord32 . Funct3.toWord32

rs1
    :: Register -> InstructionChunk 15 20
rs1 = fromWord32 . Register.toWord32

rs2
    :: Register -> InstructionChunk 20 25
rs2 = fromWord32 . Register.toWord32

funct7
    :: Funct7 -> InstructionChunk 25 32
funct7 = fromWord32 . Funct7.toWord32

imm_11_0
    :: Immediate 11 -> InstructionChunk 20 32
imm_11_0 = fromWord32 . Immediate.toWord32

imm_4_0
    :: Immediate 11 -> InstructionChunk 7 12
imm_4_0 = fromWord32 . Immediate.toWord32

imm_11_5
    :: Immediate 11 -> InstructionChunk 25 32
imm_11_5 x = fromWord32 $ Immediate.toWord32 x `shiftR` 5

