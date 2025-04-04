{-# LANGUAGE TypeFamilies #-}

module Immediate where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Int (Int32)
import Data.Data (Proxy(..))

newtype Immediate (n :: Nat) = Immediate Int32

bitsize :: forall n proxy. KnownNat n => proxy n -> Int
bitsize p = fromIntegral $ natVal p 

fromInt32 :: forall n. KnownNat n => Int32 -> Maybe (Immediate n)
fromInt32 x = if abs x > mask then Nothing else
    let sign = ((x .&. 1) `shiftL` 31) `shiftR` 31 in
    let payload = x .&. (mask `shiftR` 1) in
    Just . Immediate $ (sign `shiftL` bitsize_) .|. payload 
    where
       bitsize_ = bitsize (Proxy @n)
       mask = (1 `shiftL` bitsize_) - 1
       msbMask = mask `xor` (mask `shiftR` 1)
       maskInv = complement mask 

        


