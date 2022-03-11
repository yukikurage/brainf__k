module Brainfk.System.Data.BrainfkAST where

import Prelude

import Data.Array (replicate)
import Data.String.CodeUnits (fromCharArray)

data Command
  = PointerIncrement Int
  | PointerDecrement Int
  | ReferenceIncrement Int
  | ReferenceDecrement Int
  | Output
  | Input
  | Loop Statement

derive instance Eq Command
derive instance Ord Command
instance Show Command where
  show (PointerIncrement i) = fromCharArray $ replicate i '>'
  show (PointerDecrement i) = fromCharArray $ replicate i '<'
  show (ReferenceIncrement i) = fromCharArray $ replicate i '+'
  show (ReferenceDecrement i) = fromCharArray $ replicate i '-'
  show Output = "."
  show Input = ","
  show (Loop s) = "[" <> show s <> "]"

data Statement = StatementCont Command Statement | StatementEnd

derive instance Eq Statement
derive instance Ord Statement
instance Show Statement where
  show (StatementCont c s) = show c <> "" <> show s
  show StatementEnd = ""

newtype BrainfkAST = BrainfkAST Statement

derive instance Eq BrainfkAST
derive instance Ord BrainfkAST
instance Show BrainfkAST where
  show (BrainfkAST s) = show s
