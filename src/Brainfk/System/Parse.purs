module Brainfk.System.Parse where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Brainfk.System.Data.Parser (ParseError(..), Parser, Token(..), runParser)
import Brainfk.Util (whileM)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (get, put)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt, fromCharArray, length, slice)

type Tokens =
  { referenceIncrement :: String
  , referenceDecrement :: String
  , pointerIncrement :: String
  , pointerDecrement :: String
  , output :: String
  , input :: String
  , loopStart :: String
  , loopEnd :: String
  }

defaultToken :: Tokens
defaultToken =
  { referenceIncrement: "+"
  , referenceDecrement: "-"
  , pointerIncrement: ">"
  , pointerDecrement: "<"
  , output: "."
  , input: ","
  , loopStart: "["
  , loopEnd: "]"
  }

kurageToken :: Tokens
kurageToken =
  { referenceIncrement: "ଦ"
  , referenceDecrement: "ନ"
  , pointerIncrement: "ଲ"
  , pointerDecrement: "କ"
  , output: "ଳ"
  , input: "ଵ"
  , loopStart: "ଥ"
  , loopEnd: "ଧ"
  }

{-
ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଥ ନ ଲ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଲ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଲ ଦ ଦ ଦ ଦ ଦ କ କ କ ଧ ଲ ଳ ଲ ଦ ଦ ଳ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଳ ଳ ଦ ଦ ଦ ଳ ଲ ନ ଳ
ନ ନ ନ ନ ନ ନ ନ ନ ନ ନ ନ ନ ଳ କ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଦ ଳ ନ ନ ନ ନ ନ ନ ନ ନ ଳ ଦ ଦ ଦ ଳ ନ ନ ନ ନ ନ ନ ଳ ନ ନ ନ ନ ନ ନ ନ ନ ଳ ଲ ଦ ଳ
-}

parse :: Tokens -> String -> Either ParseError BrainfkAST
parse tokens str = BrainfkAST <$> runParser
  (pMany pSpace *> pStatement tokens <* pEOF)
  str

pToken :: String -> Parser String
pToken expected = do
  { input, position } <- get
  let
    len = length expected
  case slice position (position + len) input of
    Just res | res == expected -> do
      put $ { input, position: position + len }
      pure expected
    Just res -> throwError $ UnexpectedToken position (TokenString expected) $
      TokenString res
    Nothing -> throwError $ UnexpectedToken position (TokenString expected)
      TokenEOF

try :: forall a. Parser a -> Parser a
try p = do
  prev <- get
  catchError p \e -> do
    put prev
    throwError e

pEOF :: Parser Unit
pEOF = do
  { input, position } <- get
  case charAt position input of
    Nothing -> pure unit
    Just c -> throwError $ UnexpectedToken position TokenEOF $ TokenString $
      fromCharArray [ c ]

pSpace :: Parser Unit
pSpace =
  (pToken " " <|> pToken "\t" <|> pToken "\n" <|> pToken "\r") *> pure unit

pMany :: forall a. Parser a -> Parser (Array a)
pMany p = whileM $ (Just <$> try p) <|> pure Nothing

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* pMany pSpace

pSome :: forall a. Parser a -> Parser (Array a)
pSome p = do
  x <- p
  xs <- pMany p
  pure $ Array.cons x xs

pStatement :: Tokens -> Parser Statement
pStatement tokens = pStatementCont <|> pStatementEnd
  where
  pStatementEnd = pure StatementEnd
  pStatementCont = do
    command <- pCommand tokens
    statement <- pStatement tokens
    pure $ StatementCont command statement

pCommand :: Tokens -> Parser Command
pCommand tokens = pCommandReferenceIncrement
  <|> pCommandReferenceDecrement
  <|> pCommandPointerIncrement
  <|> pCommandPointerDecrement
  <|> pCommandOutput
  <|> pCommandInput
  <|> pCommandLoop
  where
  pCommandReferenceIncrement = ReferenceIncrement <$>
    (Array.length <$> pSome (lexeme $ pToken tokens.referenceIncrement))
  pCommandReferenceDecrement = ReferenceIncrement <$>
    ( negate <<< Array.length <$> pSome
        (lexeme $ pToken tokens.referenceDecrement)
    )
  pCommandPointerIncrement = PointerIncrement <$>
    (Array.length <$> pSome (lexeme $ pToken tokens.pointerIncrement))
  pCommandPointerDecrement = PointerIncrement <$>
    ( negate <<< Array.length <$> pSome
        (lexeme $ pToken tokens.pointerDecrement)
    )
  pCommandOutput = Output <$ (lexeme $ pToken tokens.output)
  pCommandInput = Input <$ (lexeme $ pToken tokens.input)
  pCommandLoop = do
    _ <- lexeme $ pToken tokens.loopStart
    statement <- pStatement tokens
    _ <- lexeme $ pToken tokens.loopEnd
    pure $ Loop statement
