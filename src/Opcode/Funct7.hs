{-# LANGUAGE DerivingStrategies #-}

module Opcode.Funct7
  ( Funct7
  , fromWord32
  , toWord32
  , zero
  , subSra
  ) where

import Data.Bits ((.&.))
import Data.Word (Word32)

-- Funct7 module
newtype Funct7 = Funct7 Word32
  deriving newtype (Eq, Show)

funct7Mask :: Word32
funct7Mask = 0b1111111

fromWord32 :: Word32 -> Funct7
fromWord32 x = Funct7 (x .&. funct7Mask)

toWord32 :: Funct7 -> Word32
toWord32 (Funct7 x) = x

-- Funct7 constants
zero, subSra :: Funct7
zero = fromWord32 0b0000000
subSra = fromWord32 0b0100000
