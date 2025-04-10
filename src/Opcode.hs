{-# LANGUAGE DerivingStrategies #-}

module Opcode
  ( Opcode
  , fromWord32
  , toWord32
  , arithRegImmOpcode
  , arithRegRegOpcode
  , luiOpcode
  , auipcOpcode
  , branchOpcode
  , jalOpcode
  , jalrOpcode
  , systemOpcode
  , loadOpcode
  , storeOpcode
  , fenceOpcode
  ) where

import Data.Bits ((.&.))
import Data.Word (Word32)

-- Opcode module
newtype Opcode = Opcode Word32
  deriving newtype (Eq, Show)

opcodeMask :: Word32
opcodeMask = 0b1111111

fromWord32 :: Word32 -> Opcode
fromWord32 x = Opcode (x .&. opcodeMask)

toWord32 :: Opcode -> Word32
toWord32 (Opcode x) = x

-- Opcode constants
arithRegImmOpcode
  , arithRegRegOpcode
  , luiOpcode
  , auipcOpcode
  , branchOpcode
  , jalOpcode
  , jalrOpcode
  , systemOpcode
  , loadOpcode
  , storeOpcode
  , fenceOpcode
    :: Opcode
arithRegImmOpcode = fromWord32 0b0010011
arithRegRegOpcode = fromWord32 0b0110011
luiOpcode = fromWord32 0b0110111
auipcOpcode = fromWord32 0b0010111
branchOpcode = fromWord32 0b1100011
jalOpcode = fromWord32 0b1101111
jalrOpcode = fromWord32 0b1100111
systemOpcode = fromWord32 0b1110011
loadOpcode = fromWord32 0b0000011
storeOpcode = fromWord32 0b0100011
fenceOpcode = fromWord32 0b0001111
