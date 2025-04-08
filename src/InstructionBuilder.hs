{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module InstructionBuilder () where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Kind (Type)
import Data.Type.Bool (type (&&), type (||))
import Data.Word (Word32)

import Opcode.Funct3 (Funct3)
import qualified Opcode.Funct3 as Funct3
import Opcode (Opcode)
import qualified Opcode 
import Register (Register)
import qualified Register

newtype PartialInstruction a = PartialInstruction Word32

type BuilderStateSig :: *
type BuilderStateSig = (Bool, Bool, Bool, Bool, Bool, Bool)

type BuilderState :: BuilderStateSig -> Type
type family BuilderState n where
    BuilderState n = PartialInstruction n

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
    :: Opcode -> BuilderState '(False, False, False, False, False, True)
opcode = PartialInstruction . Opcode.toWord32 

rd
    :: Register -> BuilderState '(False, False, False, False, True, False)
rd reg = PartialInstruction $ Register.toWord32 reg `shiftL` 7

funct3
    :: Funct3 -> BuilderState '(False, False, False, True, False, False)
funct3 = PartialInstruction . Funct3.toWord32

rs1
    :: Register -> BuilderState '(False, False, True, False, False, False)
rs1 reg = PartialInstruction $ Register.toWord32 reg `shiftL` 15

rs2
    :: Register -> BuilderState '(False, False, True, False, False, False)
rs2 reg = PartialInstruction $ Register.toWord32 reg `shiftL` 20

