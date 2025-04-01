{-# LANGUAGE DefaultSignatures #-}
module Immediate (Immediate(fromInt32, toInt32)) where

import Data.Bits (shiftR, (.&.), complement, xor)
import Data.Int (Int32)

class (Show a, Eq a) => Immediate a where
    mask :: Maybe a -> Int32
    msbMask :: Maybe a -> Int32 
    maskInv :: Maybe a -> Int32 
    fromInt32Unchecked :: Int32 -> a
    fromInt32 :: Int32 -> Maybe a
    toInt32 :: a -> Int32

    default msbMask :: Maybe a -> Int32
    msbMask x = (mask x `shiftR` 1) ^ mask x

    default maskInv :: Maybe a -> Int32
    maskInv = complement . mask 

    default fromInt32 :: Int32 -> Maybe a
    fromInt32 = (fromInt32Unchecked <$> ) . 
      ofInt32 ((mask :: Maybe a -> Int32) Nothing) 
      ((maskInv :: Maybe a -> Int32) Nothing)

newtype Immediate12 = Immediate12 Int32 
    deriving newtype (Show, Eq)

instance Immediate Immediate12 where
    mask _ = 0xFFF
    fromInt32Unchecked = Immediate12
    toInt32 (Immediate12 x) = x

newtype Immediate20 = Immediate20 Int32
    deriving newtype (Show, Eq)

instance Immediate Immediate20 where
    mask _ = 0x1FFFFF
    fromInt32Unchecked = Immediate20
    toInt32 (Immediate20 x) = x

ofInt32 :: Int32 -> Int32 -> Int32 -> Maybe Int32
ofInt32 mask maskInv x =
  let maskHalf = mask `shiftR` 1
  in if x < 0 then
       if abs x > maskHalf
       then Nothing
       else Just (x `xor` maskInv)
     else if x > maskHalf
          then Nothing
          else Just x

