{-# LANGUAGE DerivingStrategies #-}

module Opcode.Funct7
    ( Funct7
    , ofWord32Funct7
    , toWord32Funct7
    , zero
    , subSra
    ) where

import Data.Bits (shiftL, (.&.))
import Data.Word (Word32)

-- Funct7 module
newtype Funct7 = Funct7 Word32
    deriving newtype (Eq, Show)

funct7Mask :: Word32
funct7Mask = 0b1111111

ofWord32Funct7 :: Word32 -> Funct7
ofWord32Funct7 x = Funct7 ((x .&. funct7Mask) `shiftL` 25)

toWord32Funct7 :: Funct7 -> Word32
toWord32Funct7 (Funct7 x) = x

-- Funct7 constants
zero, subSra :: Funct7
zero = ofWord32Funct7 0b0000000
subSra = ofWord32Funct7 0b0100000
