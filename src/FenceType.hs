module FenceType
  ( FenceType
  , i
  , o
  , r
  , w
  , (|+)
  , toWord32
  , show
  ) where

import Data.Bits (Bits (..))
import Data.Word (Word32)

-- The fence_type is represented as an Int
newtype FenceType = FenceType {toWord32 :: Word32}
  deriving (Eq) -- Auto-derives Eq instance

instance Show FenceType where
  show = showFenceType

-- Constants
i, o, r, w :: FenceType
i = FenceType 8
o = FenceType 4
r = FenceType 2
w = FenceType 1

-- Shows the fence type as a string of flags
showFenceType :: FenceType -> String
showFenceType (FenceType a) =
  let iFlag = if a .&. toWord32 i == toWord32 i then "i" else ""
      oFlag = if a .&. toWord32 o == toWord32 o then "o" else ""
      rFlag = if a .&. toWord32 r == toWord32 r then "r" else ""
      wFlag = if a .&. toWord32 w == toWord32 w then "w" else ""
  in  iFlag ++ oFlag ++ rFlag ++ wFlag

-- Bitwise OR operation (|+) operator
(|+) :: FenceType -> FenceType -> FenceType
(|+) (FenceType a) (FenceType b) = FenceType (a .|. b)
