{-# LANGUAGE DataKinds #-}

module Instruction.Raw where

import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import FenceType (FenceType)
import Immediate (Immediate)
import Immediate qualified
import InstructionBuilder (InstructionType (..), buildInstruction)
import Numeric (showHex)
import Opcode (Opcode)
import Opcode qualified
import Opcode.Funct3 (Funct3)
import Opcode.Funct3 qualified as Funct3
import Opcode.Funct7 (Funct7)
import Opcode.Funct7 qualified as Funct7
import Register (Register)
import Register qualified

newtype RawInstruction
  = RawInstruction {toWord32 :: Word32}
  deriving newtype (Eq)

fromWord32 :: Word32 -> RawInstruction
fromWord32 = RawInstruction

instance Show RawInstruction where
  show (RawInstruction x) = "0x" <> showHex x ""

mkROrIInstruction
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> Maybe Funct7
  -> Maybe Opcode
  -> Funct3
  -> RawInstruction
mkROrIInstruction
  rd
  rs1
  rs2OrImm
  mbFunct7
  mbOpcode
  funct3 = RawInstruction . buildInstruction $ case rs2OrImm of
    Left rs2 -> RType rd rs1 rs2 funct3 (fromMaybe Funct7.zero mbFunct7)
    Right imm -> IType mbOpcode rd rs1 imm funct3

add
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
add dst src1 src2 = mkROrIInstruction dst src1 src2 Nothing Nothing Funct3.addSub

sub :: Register -> Register -> Register -> RawInstruction
sub dst src1 src2 =
  mkROrIInstruction
    dst
    src1
    (Left src2)
    (Just Funct7.subSra)
    Nothing
    Funct3.addSub

sll
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
sll dst src1 src2 =
  mkROrIInstruction dst src1 src2 (Just Funct7.zero) Nothing Funct3.sll

srl
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
srl dst src1 src2 =
  mkROrIInstruction dst src1 src2 (Just Funct7.zero) Nothing Funct3.srl

and
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
and dst src1 src2 =
  mkROrIInstruction dst src1 src2 (Just Funct7.zero) Nothing Funct3.and

xor
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
xor dst src1 src2 =
  mkROrIInstruction dst src1 src2 (Just Funct7.zero) Nothing Funct3.xor

or
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
or dst src1 src2 = mkROrIInstruction dst src1 src2 Nothing Nothing Funct3.or

slt
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
slt dst src1 src2 = mkROrIInstruction dst src1 src2 Nothing Nothing Funct3.slt

sltu
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
sltu dst src1 src2 = mkROrIInstruction dst src1 src2 Nothing Nothing Funct3.sltu

sra
  :: Register
  -> Register
  -> Either Register (Immediate 12)
  -> RawInstruction
sra dst src1 src2 =
  mkROrIInstruction
    dst
    src1
    src2
    (Just Funct7.subSra)
    Nothing
    Funct3.srl

lui :: Register -> Immediate 20 -> RawInstruction
lui dst imm = RawInstruction . buildInstruction $ UType Opcode.luiOpcode dst imm

auipc :: Register -> Immediate 20 -> RawInstruction
auipc dst imm =
  RawInstruction . buildInstruction $ UType Opcode.auipcOpcode dst imm

lb :: Register -> Immediate 12 -> Register -> RawInstruction
lb dst offset base =
  RawInstruction . buildInstruction
    $ IType (Just Opcode.loadOpcode) dst base offset Funct3.lb

lh :: Register -> Immediate 12 -> Register -> RawInstruction
lh dst offset base =
  RawInstruction . buildInstruction
    $ IType (Just Opcode.loadOpcode) dst base offset Funct3.lh

lw :: Register -> Immediate 12 -> Register -> RawInstruction
lw dst offset base =
  RawInstruction . buildInstruction
    $ IType (Just Opcode.loadOpcode) dst base offset Funct3.lw

lbu :: Register -> Immediate 12 -> Register -> RawInstruction
lbu dst offset base =
  RawInstruction . buildInstruction
    $ IType (Just Opcode.loadOpcode) dst base offset Funct3.lbu

lhu :: Register -> Immediate 12 -> Register -> RawInstruction
lhu dst offset base =
  RawInstruction . buildInstruction
    $ IType (Just Opcode.loadOpcode) dst base offset Funct3.lhu

sb :: Register -> Immediate 12 -> Register -> RawInstruction
sb src offset base =
  RawInstruction . buildInstruction
    $ SType Opcode.storeOpcode base src offset Funct3.sb

sh :: Register -> Immediate 12 -> Register -> RawInstruction
sh src offset base =
  RawInstruction . buildInstruction
    $ SType Opcode.storeOpcode base src offset Funct3.sh

sw :: Register -> Immediate 12 -> Register -> RawInstruction
sw src offset base =
  RawInstruction . buildInstruction
    $ SType Opcode.storeOpcode base src offset Funct3.sw

beq :: Register -> Register -> Immediate 12 -> RawInstruction
beq src1 src2 offset =
  RawInstruction . buildInstruction $ BType src1 src2 offset Funct3.beq

bne :: Register -> Register -> Immediate 12 -> RawInstruction
bne src1 src2 offset =
  RawInstruction . buildInstruction $ BType src1 src2 offset Funct3.bne

blt :: Register -> Register -> Immediate 12 -> RawInstruction
blt src1 src2 offset =
  RawInstruction . buildInstruction $ BType src1 src2 offset Funct3.blt

bltu :: Register -> Register -> Immediate 12 -> RawInstruction
bltu src1 src2 offset =
  RawInstruction . buildInstruction $ BType src1 src2 offset Funct3.bltu

bge :: Register -> Register -> Immediate 12 -> RawInstruction
bge src1 src2 offset =
  RawInstruction . buildInstruction $ BType src1 src2 offset Funct3.bge

bgeu :: Register -> Register -> Immediate 12 -> RawInstruction
bgeu src1 src2 offset =
  RawInstruction . buildInstruction $ BType src1 src2 offset Funct3.bgeu

jal :: Register -> Immediate 20 -> RawInstruction
jal dst imm = RawInstruction . buildInstruction $ JType Opcode.jalOpcode dst imm

jalr :: Register -> Register -> Immediate 12 -> RawInstruction
jalr dst base offset =
  RawInstruction . buildInstruction
    $ IType (Just Opcode.jalrOpcode) dst base offset Funct3.jalr

ecall :: RawInstruction
ecall =
  RawInstruction . buildInstruction
    $ IType
      (Just Opcode.systemOpcode)
      Register.x0
      Register.x0
      (Immediate.fromWord32 0)
      Funct3.priv

ebreak :: RawInstruction
ebreak =
  RawInstruction . buildInstruction
    $ IType
      (Just Opcode.systemOpcode)
      Register.x0
      Register.x0
      (Immediate.fromWord32 1)
      Funct3.priv

fence :: FenceType -> FenceType -> RawInstruction
fence pre succ =
  RawInstruction . buildInstruction
    $ FType Opcode.fenceOpcode Funct3.fence pre succ
