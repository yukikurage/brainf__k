module Brainfk.System.Data.BrainfkAST where

import Prelude

data Command
  = PointerIncrement
  | PointerDecrement
  | ReferenceIncrement
  | ReferenceDecrement
  | Output
  | Input
  | Loop Statement

derive instance Eq Command
derive instance Ord Command
instance Show Command where
  show PointerIncrement = ">"
  show PointerDecrement = "<"
  show ReferenceIncrement = "+"
  show ReferenceDecrement = "-"
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
