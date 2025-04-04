{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module InstructionBuilder (someFunc) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Type.Bool (type (&&), type (||))
import Data.Type.Equality ((:~:) (Refl))
import Data.Word (Word32)
import GHC.TypeLits (Div, Mod, Nat)

newtype PartialInstruction a = PartialInstruction Word32

type BuilderStateSig :: *
type BuilderStateSig = (Bool, Bool, Bool, Bool, Bool, Bool)

type BuilderState :: BuilderStateSig -> Type
type family BuilderState n where
    BuilderState n = PartialInstruction n

x :: BuilderState '(True, False, False, False, False, False)
x = PartialInstruction 0

y :: BuilderState '(False, True, False, False, False, False)
y = PartialInstruction 1

(|:)
    :: ( (a1 && a2) ~ 'False
       , (b1 && b2) ~ 'False
       , (c1 && c2) ~ 'False
       , (d1 && d2) ~ 'False
       , (e1 && e2) ~ 'False
       , (f1 && f2) ~ 'False
       )
    => BuilderState '(a1, b1, c1, d1, e1, f1)
    -> BuilderState '(a2, b2, c2, d2, e2, f2)
    -> BuilderState
        '(a1 || a2, b1 || b2, c1 || c2, d1 || d2, e1 || e2, f1 || f2)
(|:) (PartialInstruction x) (PartialInstruction y) = PartialInstruction (x .|. y)

opcode
    :: Word32 -> BuilderState '(False, False, False, False, False, True)
opcode x = PartialInstruction (x .&. 0b00000000000000000000000001111111)

rd
    :: Word32 -> BuilderState '(False, False, False, False, True, False)
rd x = PartialInstruction (x .&. (0b11111 `shiftL` 7))

funct3
    :: Word32 -> BuilderState '(False, False, False, True, False, False)
funct3 x = PartialInstruction (x .&. (0b111 `shiftL` 12))

rs1
    :: Word32 -> BuilderState '(False, False, True, False, False, False)
rs1 x = PartialInstruction (x .&. (0b11111 `shiftL` 15))

z = x |: y

someFunc :: IO ()
someFunc = putStrLn "someFunc"
