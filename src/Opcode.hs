{-# LANGUAGE DerivingStrategies #-}

module Opcode 
  ( Opcode
  , ofWord32Opcode
  , toWord32Opcode
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

ofWord32Opcode :: Word32 -> Opcode
ofWord32Opcode x = Opcode (x .&. opcodeMask)

toWord32Opcode :: Opcode -> Word32
toWord32Opcode (Opcode x) = x

-- Opcode constants
arithRegImmOpcode, arithRegRegOpcode, luiOpcode, auipcOpcode, branchOpcode,
  jalOpcode, jalrOpcode, systemOpcode, loadOpcode, storeOpcode, fenceOpcode :: Opcode
arithRegImmOpcode = ofWord32Opcode 0b0010011
arithRegRegOpcode = ofWord32Opcode 0b0110011
luiOpcode         = ofWord32Opcode 0b0110111
auipcOpcode       = ofWord32Opcode 0b0010111
branchOpcode      = ofWord32Opcode 0b1100011
jalOpcode         = ofWord32Opcode 0b1101111
jalrOpcode        = ofWord32Opcode 0b1100111
systemOpcode      = ofWord32Opcode 0b1110011
loadOpcode        = ofWord32Opcode 0b0000011
storeOpcode       = ofWord32Opcode 0b0100011
fenceOpcode       = ofWord32Opcode 0b0001111
