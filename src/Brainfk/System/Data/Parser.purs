module Brainfk.System.Data.Parser where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, evalStateT)
import Data.Array (intercalate)
import Data.Either (Either)
import Data.Tuple.Nested ((/\))

data Token = TokenString String | TokenEOF

derive instance Eq Token
derive instance Ord Token
instance Show Token where
  show (TokenString str) = "\"" <> str <> "\""
  show TokenEOF = "EOF"

data ParseError = UnexpectedToken Int (Array Token) Token

derive instance Eq ParseError
derive instance Ord ParseError
instance Show ParseError where
  show (UnexpectedToken position expected token) =
    "Unexpected token at position "
      <> show position
      <> ": "
      <> show token
      <> " (expected "
      <> intercalate ", " (map show expected)
      <> ")"

instance Semigroup ParseError where
  append (UnexpectedToken pos1 exp1 act1) (UnexpectedToken pos2 exp2 act2) =
    if pos1 /\ act1 == pos2 /\ act2 then UnexpectedToken pos1 (exp1 <> exp2)
      act1
    else UnexpectedToken pos2 exp2 act2

type ParserState =
  { input :: String
  , position :: Int
  }

newtype Parser a = Parser (StateT ParserState (Except ParseError) a)

derive newtype instance Functor Parser
derive newtype instance Apply Parser
derive newtype instance Alt Parser
derive newtype instance Applicative Parser
derive newtype instance Bind Parser
derive newtype instance MonadThrow ParseError Parser
derive newtype instance Monad Parser
derive newtype instance MonadState ParserState Parser
derive newtype instance MonadRec Parser
derive newtype instance MonadError ParseError Parser

runParser :: forall a. Parser a -> String -> Either ParseError a
runParser (Parser p) input = runExcept $ evalStateT p
  { input
  , position: 0
  }
