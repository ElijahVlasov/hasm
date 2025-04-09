{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module InstructionBuilder (InstructionType (..), buildInstruction) where

import Data.Bits (shiftR)

import Data.Word (Word32)
import FenceType (FenceType)
import Immediate (Immediate)
import Immediate qualified
import InstructionBuilder.InstructionChunk
  ( InstructionChunk
  , (>:>)
  )

import InstructionBuilder.InstructionChunk qualified as IChunk
import Opcode (Opcode)
import Opcode qualified
import Opcode.Funct3 (Funct3)
import Opcode.Funct3 qualified as Funct3
import Opcode.Funct7 (Funct7)
import Opcode.Funct7 qualified as Funct7
import Register (Register)
import Register qualified

opcode
  :: Opcode -> InstructionChunk 0 7
opcode = IChunk.fromWord32 . Opcode.toWord32

rd
  :: Register -> InstructionChunk 7 12
rd = IChunk.fromWord32 . Register.toWord32

funct3
  :: Funct3 -> InstructionChunk 12 15
funct3 = IChunk.fromWord32 . Funct3.toWord32

rs1
  :: Register -> InstructionChunk 15 20
rs1 = IChunk.fromWord32 . Register.toWord32

rs2
  :: Register -> InstructionChunk 20 25
rs2 = IChunk.fromWord32 . Register.toWord32

funct7
  :: Funct7 -> InstructionChunk 25 32
funct7 = IChunk.fromWord32 . Funct7.toWord32

data InstructionType
  = RType
      { rtRd :: Register
      , rtRs1 :: Register
      , rtRs2 :: Register
      , rtFunct3 :: Funct3
      , rtFunct7 :: Funct7
      }
  | IType
      { itOpcode :: Maybe Opcode
      , itRd :: Register
      , itRs1 :: Register
      , itImm :: Immediate 12
      , itFunct3 :: Funct3
      }
  | SType
      { stOpcode :: Opcode
      , stRs1 :: Register
      , stRs2 :: Register
      , stImm :: Immediate 12
      , stFunct3 :: Funct3
      }
  | BType
      { btRs1 :: Register
      , btRs2 :: Register
      , btImm :: Immediate 12
      , btFunct3 :: Funct3
      }
  | UType
      { utOpcode :: Opcode
      , utRd :: Register
      , utImm :: Immediate 20
      }
  | JType
      { jtOpcode :: Opcode
      , jtRd :: Register
      , jtImm :: Immediate 20
      }
  | FType
      { ftOpcode :: Opcode
      , ftFunct3 :: Funct3
      , ftPre :: FenceType
      , ftSucc :: FenceType
      }

buildInstruction
  :: InstructionType -> Word32
buildInstruction (RType rtRd rtRs1 rtRs2 rtFunct3 rtFunct7) =
  IChunk.toWord32
    $ opcode Opcode.arithRegRegOpcode
      >:> rd rtRd
      >:> funct3 rtFunct3
      >:> rs1 rtRs1
      >:> rs2 rtRs2
      >:> funct7 rtFunct7

