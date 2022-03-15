module Brainfk.System.Data.BrainfkAST where

import Prelude

import Data.Array (fold, replicate)

data Command
  = PointerIncrement Int Int
  | ReferenceIncrement Int Int
  | Output Int
  | Input Int
  | Loop Int Statement

derive instance Eq Command
derive instance Ord Command
instance Show Command where
  show (PointerIncrement _ i) = fold $ replicate i ">"
  show (ReferenceIncrement _ i) = fold $ replicate i "+"
  show (Output _) = "."
  show (Input _) = ","
  show (Loop _ s) = "[" <> show s <> "]"

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
