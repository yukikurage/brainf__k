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

type Settings r =
  ( referenceIncrement :: String
  , referenceDecrement :: String
  , pointerIncrement :: String
  , pointerDecrement :: String
  , output :: String
  , input :: String
  , loopStart :: String
  , loopEnd :: String
  | r
  )

defaultSettings :: Record (Settings ())
defaultSettings =
  { referenceIncrement: "+"
  , referenceDecrement: "-"
  , pointerIncrement: ">"
  , pointerDecrement: "<"
  , output: "."
  , input: ","
  , loopStart: "["
  , loopEnd: "]"
  }

kurageToken :: Record (Settings ())
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

parse :: forall r. Record (Settings r) -> String -> Either ParseError BrainfkAST
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
    Just res -> throwError $ UnexpectedToken position [ TokenString expected ] $
      TokenString res
    Nothing -> throwError $ UnexpectedToken position [ TokenString expected ]
      TokenEOF

lookForeword :: forall a. Parser a -> Parser a
lookForeword p = do
  prev <- get
  res <- catchError p \e -> do
    put prev
    throwError e
  put prev
  pure res

try :: forall a. Parser a -> Parser a
try p = do
  prev <- get
  res <- catchError p \e -> do
    put prev
    throwError e
  pure res

pEOF :: Parser Unit
pEOF = do
  { input, position } <- get
  case charAt position input of
    Nothing -> pure unit
    Just c -> throwError $ UnexpectedToken position [ TokenEOF ] $ TokenString $
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

pStatement :: forall r. Record (Settings r) -> Parser Statement
pStatement tokens = pStatementEnd <|> pStatementCont
  where
  pStatementEnd = StatementEnd <$
    ( pEOF <|>
        ( unit <$
            (lookForeword $ lexeme $ pToken $ tokens.loopEnd)
        )
    )
  pStatementCont = do
    command <- pCommand tokens
    statement <- pStatement tokens
    pure $ StatementCont command statement

pCommand :: forall r. Record (Settings r) -> Parser Command
pCommand tokens = do
  { position } <- get
  let
    pCommandReferenceIncrement = ReferenceIncrement position <<< Array.length
      <$> pSome
        (lexeme $ pToken tokens.referenceIncrement)
    pCommandReferenceDecrement =
      ReferenceIncrement position <<< negate <<< Array.length
        <$> pSome
          (lexeme $ pToken tokens.referenceDecrement)
    pCommandPointerIncrement = PointerIncrement position <<< Array.length <$>
      pSome
        (lexeme $ pToken tokens.pointerIncrement)
    pCommandPointerDecrement = PointerIncrement position <<< negate <<< Array.length <$>
      pSome
        (lexeme $ pToken tokens.pointerDecrement)
    pCommandOutput = Output position <$ (lexeme $ pToken tokens.output)
    pCommandInput = Input position <$ (lexeme $ pToken tokens.input)
    pCommandLoop = try do
      _ <- lexeme $ pToken tokens.loopStart
      statement <- pStatement tokens
      _ <- lexeme $ pToken tokens.loopEnd
      pure $ Loop position statement
  pCommandReferenceIncrement
    <|> pCommandReferenceDecrement
    <|> pCommandPointerIncrement
    <|> pCommandPointerDecrement
    <|> pCommandOutput
    <|> pCommandInput
    <|> pCommandLoop
