{-# LANGUAGE DerivingStrategies #-}
module Opcode.Funct3 (
    Funct3,
    ofWord32Funct3,
    toWord32Funct3,
    addSub,
    Opcode.Funct3.xor,
    Opcode.Funct3.or,
    Opcode.Funct3.and,
    sll,
    srl,
    slt,
    sltu,
    lb,
    lh,
    lw,
    lbu,
    lhu,
    sb,
    sh,
    sw,
    beq,
    bne,
    blt,
    bge,
    bltu,
    bgeu,
    jalr,
    priv,
    fence
) where

import Data.Bits ((.&.), shiftL)
import Data.Word (Word32)

-- Funct3 module
newtype Funct3 = Funct3 Word32
  deriving newtype (Eq, Show)

funct3Mask :: Word32
funct3Mask = 0b111

ofWord32Funct3 :: Word32 -> Funct3
ofWord32Funct3 x = Funct3 ((x .&. funct3Mask) `shiftL` 12)

toWord32Funct3 :: Funct3 -> Word32
toWord32Funct3 (Funct3 x) = x

-- Funct3 constants
addSub, xor, or, and, sll, srl, slt, sltu, lb, lh, lw, lbu, lhu,
  sb, sh, sw, beq, bne, blt, bge, bltu, bgeu, jalr, priv, fence :: Funct3
addSub = ofWord32Funct3 0b000
xor    = ofWord32Funct3 0b100
or    = ofWord32Funct3 0b110
and   = ofWord32Funct3 0b111
sll    = ofWord32Funct3 0b001
srl    = ofWord32Funct3 0b101
slt    = ofWord32Funct3 0b010
sltu   = ofWord32Funct3 0b011
lb     = ofWord32Funct3 0b000
lh     = ofWord32Funct3 0b001
lw     = ofWord32Funct3 0b010
lbu    = ofWord32Funct3 0b100
lhu    = ofWord32Funct3 0b101
sb     = ofWord32Funct3 0b000
sh     = ofWord32Funct3 0b001
sw     = ofWord32Funct3 0b010
beq    = ofWord32Funct3 0b000
bne    = ofWord32Funct3 0b001
blt    = ofWord32Funct3 0b100
bge    = ofWord32Funct3 0b101
bltu   = ofWord32Funct3 0b110
bgeu   = ofWord32Funct3 0b111
jalr   = ofWord32Funct3 0b000
priv   = ofWord32Funct3 0b000
fence  = ofWord32Funct3 0b000
