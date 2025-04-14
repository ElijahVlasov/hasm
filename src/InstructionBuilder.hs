{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module InstructionBuilder (InstructionType (..), buildInstruction) where

import Data.Bits (shiftR)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import GHC.TypeLits (CmpNat, KnownNat)
import GHC.TypeNats (type (+))

import FenceType (FenceType)
import FenceType qualified
import Immediate (Immediate)
import Immediate qualified
import InstructionBuilder.InstructionChunk
  ( InstructionChunk
  , (>:>)
  )
import InstructionBuilder.InstructionChunk qualified as IChunk
import Opcode (Opcode)
import Opcode qualified
import Opcode qualified as Opcodes
import Opcode.Funct3 (Funct3)
import Opcode.Funct3 qualified as Funct3
import Opcode.Funct7 (Funct7)
import Opcode.Funct7 qualified as Funct7
import Register (Register)
import Register qualified

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
      , jtImm :: Immediate 21
      }
  | FType
      { ftOpcode :: Opcode
      , ftFunct3 :: Funct3
      , ftPre :: FenceType
      , ftSucc :: FenceType
      }

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

imm
  :: (KnownNat n, KnownNat m, KnownNat (n + m), CmpNat n (n + m) ~ 'LT)
  => Immediate m
  -> InstructionChunk n (n + m)
imm = IChunk.fromWord32 . Immediate.toWord32

fence :: FenceType -> FenceType -> InstructionChunk 20 28
fence pre post =
  ( IChunk.fromWord32 (FenceType.toWord32 post)
      :: InstructionChunk 20 24
  )
    >:> IChunk.fromWord32 (FenceType.toWord32 pre)

buildInstruction
  :: InstructionType -> Word32
buildInstruction (RType rtRd rtRs1 rtRs2 rtFunct3 rtFunct7) =
  IChunk.toWord32
    $ opcode Opcodes.arithRegRegOpcode
      >:> rd rtRd
      >:> funct3 rtFunct3
      >:> rs1 rtRs1
      >:> rs2 rtRs2
      >:> funct7 rtFunct7
buildInstruction
  (IType itOpcode itRd itRs1 itImm itFunct3) =
    IChunk.toWord32
      $ opcode
        (fromMaybe Opcodes.arithRegImmOpcode itOpcode)
        >:> rd itRd
        >:> funct3 itFunct3
        >:> rs1 itRs1
        >:> imm itImm
buildInstruction
  (SType stOpcode stRs1 stRs2 stImm stFunct3) =
    let imm = Immediate.toWord32 stImm
    in  IChunk.toWord32
          $ opcode stOpcode
            >:> IChunk.fromWord32 imm
            >:> funct3 stFunct3
            >:> rs1 stRs1
            >:> rs2 stRs2
            >:> IChunk.fromWord32 (imm `shiftR` 5)
buildInstruction
  (UType utOpcode utRd utImm) =
    IChunk.toWord32
      $ opcode utOpcode
        >:> rd utRd
        >:> imm utImm
buildInstruction
  (BType btRs1 btRs2 btImm btFunct3) =
    let imm = Immediate.toWord32 btImm
    in  IChunk.toWord32
          $ opcode Opcodes.branchOpcode
            >:> IChunk.writeByte (imm `shiftR` 11)
            >:> IChunk.fromWord32 (imm `shiftR` 1)
            >:> funct3 btFunct3
            >:> rs1 btRs1
            >:> rs2 btRs2
            >:> IChunk.fromWord32 (imm `shiftR` 5)
            >:> IChunk.writeByte (imm `shiftR` 12)
buildInstruction
  (JType jtOpcode jtRd jtImm) =
    let imm = Immediate.toWord32 jtImm
    in  IChunk.toWord32
          $ opcode jtOpcode
            >:> rd jtRd
            >:> (IChunk.fromWord32 (imm `shiftR` 12) :: InstructionChunk 12 20)
            >:> IChunk.writeByte (imm `shiftR` 11)
            >:> IChunk.fromWord32 (imm `shiftR` 1)
            >:> IChunk.writeByte (imm `shiftR` 20)
buildInstruction
  (FType ftOpcode ftFunct3 ftPre ftSucc) =
    IChunk.toWord32
      $ opcode ftOpcode
        >:> rd Register.x0
        >:> funct3 ftFunct3
        >:> rs1 Register.x0
        >:> fence ftPre ftSucc
        >:> IChunk.fromWord32 0
