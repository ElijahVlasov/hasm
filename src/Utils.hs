module Utils (int32ToWord32, word32ToInt32) where

import Data.Int (Int32)
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

int32ToWord32 :: Int32 -> Word32
int32ToWord32 = unsafeCoerce

word32ToInt32 :: Word32 -> Int32
word32ToInt32 = unsafeCoerce
