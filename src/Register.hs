{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Register
    ( Register
    , ofInt
    , ofInt32
    , toInt
    , toInt32
    , toString
    , ofString
    , equalRegister
    , ppRegister
    -- Register constants
    , x0
    , x1
    , x2
    , x3
    , x4
    , x5
    , x6
    , x7
    , x8
    , x9
    , x10
    , x11
    , x12
    , x13
    , x14
    , x15
    , x16
    , x17
    , x18
    , x19
    , x20
    , x21
    , x22
    , x23
    , x24
    , x25
    , x26
    , x27
    , x28
    , x29
    , x30
    , x31
    , x32
    , pc
    , zero
    , ra
    , sp
    , gp
    , tp
    , t0
    , t1
    , t2
    , s0
    , s1
    , a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , s2
    , s3
    , s4
    , s5
    , s6
    , s7
    , s8
    , s9
    , s10
    , s11
    , t3
    , t4
    , t5
    , t6
    ) where

import Data.Int (Int32)
import Data.Text (Text, pack)
import Text.Printf (printf)

-- Register type
newtype Register = Register Int32
    deriving newtype (Eq)

instance Show Register where
    show (Register n) = printf "x%d" (toInt (Register n))

-- Conversion functions
ofInt :: Int -> Maybe Register
ofInt n =
    if n < 0 || n > 31 then Nothing else Just (Register (fromIntegral n))

ofInt32 :: Int32 -> Maybe Register
ofInt32 n = if n < 0 || n > 31 then Nothing else Just (Register n)

toInt :: Register -> Int
toInt (Register n) = fromIntegral n

toInt32 :: Register -> Int32
toInt32 (Register n) = n

toString :: Register -> Text
toString (Register n) = pack $ printf "x%d" (toInt (Register n))

ppRegister :: Register -> Text
ppRegister = toString

equalRegister :: Register -> Register -> Bool
equalRegister = (==)

-- Register constants
x0
    , x1
    , x2
    , x3
    , x4
    , x5
    , x6
    , x7
    , x8
    , x9
    , x10
    , x11
    , x12
    , x13
    , x14
    , x15
    , x16
    , x17
    , x18
    , x19
    , x20
    , x21
    , x22
    , x23
    , x24
    , x25
    , x26
    , x27
    , x28
    , x29
    , x30
    , x31
    , x32
    , pc
    , zero
    , ra
    , sp
    , gp
    , tp
    , t0
    , t1
    , t2
    , s0
    , s1
    , a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , s2
    , s3
    , s4
    , s5
    , s6
    , s7
    , s8
    , s9
    , s10
    , s11
    , t3
    , t4
    , t5
    , t6
        :: Register
x0 = Register 0
x1 = Register 1
x2 = Register 2
x3 = Register 3
x4 = Register 4
x5 = Register 5
x6 = Register 6
x7 = Register 7
x8 = Register 8
x9 = Register 9
x10 = Register 10
x11 = Register 11
x12 = Register 12
x13 = Register 13
x14 = Register 14
x15 = Register 15
x16 = Register 16
x17 = Register 17
x18 = Register 18
x19 = Register 19
x20 = Register 20
x21 = Register 21
x22 = Register 22
x23 = Register 23
x24 = Register 24
x25 = Register 25
x26 = Register 26
x27 = Register 27
x28 = Register 28
x29 = Register 29
x30 = Register 30
x31 = Register 31
x32 = Register 32
pc = x32
zero = x0
ra = x1
sp = x2
gp = x3
tp = x4
t0 = x5
t1 = x6
t2 = x7
s0 = x8
s1 = x9
a0 = x10
a1 = x11
a2 = x12
a3 = x13
a4 = x14
a5 = x15
a6 = x16
a7 = x17
s2 = x18
s3 = x19
s4 = x20
s5 = x21
s6 = x22
s7 = x23
s8 = x24
s9 = x25
s10 = x26
s11 = x27
t3 = x28
t4 = x29
t5 = x30
t6 = x31

-- String to register conversion
ofString :: Text -> Maybe Register
ofString s = case s of
    "x0" -> Just x0
    "x1" -> Just x1
    "x2" -> Just x2
    "x3" -> Just x3
    "x4" -> Just x4
    "x5" -> Just x5
    "x6" -> Just x6
    "x7" -> Just x7
    "x8" -> Just x8
    "x9" -> Just x9
    "x10" -> Just x10
    "x11" -> Just x11
    "x12" -> Just x12
    "x13" -> Just x13
    "x14" -> Just x14
    "x15" -> Just x15
    "x16" -> Just x16
    "x17" -> Just x17
    "x18" -> Just x18
    "x19" -> Just x19
    "x20" -> Just x20
    "x21" -> Just x21
    "x22" -> Just x22
    "x23" -> Just x23
    "x24" -> Just x24
    "x25" -> Just x25
    "x26" -> Just x26
    "x27" -> Just x27
    "x28" -> Just x28
    "x29" -> Just x29
    "x30" -> Just x30
    "x31" -> Just x31
    "x32" -> Just x32
    "pc" -> Just pc
    "zero" -> Just zero
    "ra" -> Just ra
    "sp" -> Just sp
    "gp" -> Just gp
    "tp" -> Just tp
    "t0" -> Just t0
    "t1" -> Just t1
    "t2" -> Just t2
    "s0" -> Just s0
    "s1" -> Just s1
    "a0" -> Just a0
    "a1" -> Just a1
    "a2" -> Just a2
    "a3" -> Just a3
    "a4" -> Just a4
    "a5" -> Just a5
    "a6" -> Just a6
    "a7" -> Just a7
    "s2" -> Just s2
    "s3" -> Just s3
    "s4" -> Just s4
    "s5" -> Just s5
    "s6" -> Just s6
    "s7" -> Just s7
    "s8" -> Just s8
    "s9" -> Just s9
    "s10" -> Just s10
    "s11" -> Just s11
    "t3" -> Just t3
    "t4" -> Just t4
    "t5" -> Just t5
    "t6" -> Just t6
    _ -> Nothing
