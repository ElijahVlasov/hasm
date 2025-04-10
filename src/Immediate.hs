{-# LANGUAGE TypeFamilies #-}

module Immediate (Immediate, fromInt32, fromWord32, toInt32, toWord32) where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Data (Proxy (..))
import Data.Int (Int32)
import Data.Word (Word32)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Utils (int32ToWord32, word32ToInt32)

newtype Immediate (n :: Nat) = Immediate {toWord32 :: Word32}
  deriving newtype (Eq)

instance KnownNat n => Show (Immediate n) where
  show = show . toInt32

fromWord32 :: forall n. KnownNat n => Word32 -> Immediate n
fromWord32 x = Immediate x :: Immediate n

bitsize :: forall n proxy. KnownNat n => proxy n -> Int
bitsize p = fromIntegral $ natVal p

mask :: forall n proxy. KnownNat n => proxy n -> Word32
mask p = (1 `shiftL` bitsize p) - 1

fromInt32 :: forall n. KnownNat n => Int32 -> Maybe (Immediate n)
fromInt32 x
  | x < 0 =
      if (-x) > (maskInt32 `shiftR` 1)
        then Nothing
        else Just . Immediate $ xWord `xor` maskInv
  | x > (maskInt32 `shiftR` 1) = Nothing
  | otherwise = Just $ Immediate xWord
 where
  xWord = int32ToWord32 x
  mask_ = mask (Proxy @n)
  maskInt32 = word32ToInt32 mask_
  maskInv = complement mask_

toInt32 :: forall n. KnownNat n => Immediate n -> Int32
toInt32 (Immediate imm) = if isNegative imm then immInt32 .|. maskInvInt32 else immInt32
 where
  immInt32 = word32ToInt32 imm
  mask_ = mask (Proxy @n)
  msbMask = mask_ `xor` (mask_ `shiftR` 1)
  maskInv = complement mask_
  maskInvInt32 = word32ToInt32 maskInv
  isNegative word = (word .&. msbMask) /= 0
