module Opcode.Funct3
    ( Funct3
    , fromWord32
    , toWord32
    , addSub
    , Opcode.Funct3.xor
    , Opcode.Funct3.or
    , Opcode.Funct3.and
    , sll
    , srl
    , slt
    , sltu
    , lb
    , lh
    , lw
    , lbu
    , lhu
    , sb
    , sh
    , sw
    , beq
    , bne
    , blt
    , bge
    , bltu
    , bgeu
    , jalr
    , priv
    , fence
    ) where

import Data.Bits (shiftL, (.&.))
import Data.Word (Word32)

-- Funct3 module
newtype Funct3 = Funct3 Word32
    deriving newtype (Eq, Show)

funct3Mask :: Word32
funct3Mask = 0b111

fromWord32 :: Word32 -> Funct3
fromWord32 x = Funct3 ((x .&. funct3Mask) `shiftL` 12)

toWord32 :: Funct3 -> Word32
toWord32 (Funct3 x) = x

-- Funct3 constants
addSub
    , xor
    , or
    , and
    , sll
    , srl
    , slt
    , sltu
    , lb
    , lh
    , lw
    , lbu
    , lhu
    , sb
    , sh
    , sw
    , beq
    , bne
    , blt
    , bge
    , bltu
    , bgeu
    , jalr
    , priv
    , fence
        :: Funct3
addSub = fromWord32 0b000
xor = fromWord32 0b100
or = fromWord32 0b110
and = fromWord32 0b111
sll = fromWord32 0b001
srl = fromWord32 0b101
slt = fromWord32 0b010
sltu = fromWord32 0b011
lb = fromWord32 0b000
lh = fromWord32 0b001
lw = fromWord32 0b010
lbu = fromWord32 0b100
lhu = fromWord32 0b101
sb = fromWord32 0b000
sh = fromWord32 0b001
sw = fromWord32 0b010
beq = fromWord32 0b000
bne = fromWord32 0b001
blt = fromWord32 0b100
bge = fromWord32 0b101
bltu = fromWord32 0b110
bgeu = fromWord32 0b111
jalr = fromWord32 0b000
priv = fromWord32 0b000
fence = fromWord32 0b000
