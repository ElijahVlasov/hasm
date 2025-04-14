{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module InstructionBuilder.InstructionChunk
  ( InstructionChunk
  , fromWord32
  , toWord32
  , writeByte
  , (>:>)
  ) where

import Data.Bits (Bits ((.&.), (.|.)), shiftL)
import Data.Data (Proxy (..))
import Data.Word (Word32)
import GHC.TypeLits (CmpNat, KnownNat, Nat, natVal, type (+))

newtype InstructionChunk (l :: Nat) (r :: Nat)
  = InstructionChunk {unwrap :: Word32}

fromWord32
  :: forall l r
   . (KnownNat l, KnownNat r, CmpNat l r ~ 'LT)
  => Word32
  -> InstructionChunk l r
fromWord32 x =
  InstructionChunk
    $ let lInt = (fromIntegral $ natVal (Proxy @l)) :: Int
          rInt = (fromIntegral $ natVal (Proxy @r)) :: Int
          bitsize = rInt - lInt
          mask = ((1 :: Word32) `shiftL` bitsize) - 1
      in  (x .&. mask) `shiftL` lInt

toWord32 :: InstructionChunk 0 32 -> Word32
toWord32 = unwrap

(>:>)
  :: (KnownNat l, KnownNat m, KnownNat r)
  => InstructionChunk l m
  -> InstructionChunk m r
  -> InstructionChunk l r
(>:>) (InstructionChunk a) (InstructionChunk b) = InstructionChunk (a .|. b)

writeByte
  :: forall n
   . (KnownNat n, KnownNat (n + 1), CmpNat n (n + 1) ~ 'LT)
  => Word32
  -> InstructionChunk n (n + 1)
writeByte = fromWord32
