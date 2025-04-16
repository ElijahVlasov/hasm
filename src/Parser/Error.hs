module Parser.Error (ParsingError (..)) where

import Data.Int (Int32)

data ParsingError
  = InvalidRegister String
  | InvalidImmediate Int32
  | InvalidLabel String
  | InvalidInstruction String
  | DefaultError String
  -- | InvalidSyntax (position (*) position)
  deriving (Eq, Show)
