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

parse :: String -> Either ParseError BrainfkAST
parse str = BrainfkAST <$> runParser (pStatement unit <* pEOF) str

pToken :: String -> Parser String
pToken expected = do
  { input, position } <- get
  let
    len = length expected
  case slice position (position + len) input of
    Just res | res == expected -> do
      put $ { input, position: position + len }
      pure expected
    Just res -> throwError $ UnexpectedToken position (TokenString expected) $ TokenString res
    Nothing -> throwError $ UnexpectedToken position (TokenString expected) TokenEOF

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
    Just c -> throwError $ UnexpectedToken position TokenEOF $ TokenString $ fromCharArray [ c ]

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

pStatement :: Unit -> Parser Statement
pStatement unit = pStatementCont <|> pStatementEnd
  where
  pStatementEnd = pure StatementEnd
  pStatementCont = do
    command <- pCommand
    statement <- pStatement unit
    pure $ StatementCont command statement

pCommand :: Parser Command
pCommand = pCommandReferenceIncrement
  <|> pCommandReferenceDecrement
  <|> pCommandPointerIncrement
  <|> pCommandPointerDecrement
  <|> pCommandOutput
  <|> pCommandInput
  <|> pCommandLoop
  where
  pCommandReferenceIncrement = ReferenceIncrement <$> (Array.length <$> pSome (lexeme $ pToken "+"))
  pCommandReferenceDecrement = ReferenceDecrement <$> (Array.length <$> pSome (lexeme $ pToken "-"))
  pCommandPointerIncrement = PointerIncrement <$> (Array.length <$> pSome (lexeme $ pToken ">"))
  pCommandPointerDecrement = PointerDecrement <$> (Array.length <$> pSome (lexeme $ pToken "<"))
  pCommandOutput = Output <$ (lexeme $ pToken ".")
  pCommandInput = Input <$ (lexeme $ pToken ",")
  pCommandLoop = do
    _ <- lexeme $ pToken "["
    statement <- pStatement unit
    _ <- lexeme $ pToken "]"
    pure $ Loop statement
