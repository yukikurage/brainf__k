module Brainfk.System.Exec where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Brainfk.Util (delayZero)
import Control.Monad.ST.Global (toEffect)
import Data.Array (fromFoldable, reverse)
import Data.Array.ST as AST
import Data.Char (fromCharCode, toCharCode)
import Data.List (List(..), (:))
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (abs)
import Data.String.CodeUnits (charAt, fromCharArray, length)
import Data.String.NonEmpty (toString)
import Data.String.NonEmpty.CodeUnits (snoc)
import Effect (Effect)
import Effect.Aff (Aff, Error, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

-- | OutOfRange max actual
data ExecError
  = OutOfMemoryRange Int Int
  | OutOfInputRange Int Int
  | InvalidCharCode Int
  | ExceedsMaxStep Int
  | ProcessKilled Error

instance Show ExecError where
  show (OutOfMemoryRange max actual) = "Out of memory range: " <> show actual
    <> ">"
    <> show max
  show (OutOfInputRange max actual) = "Out of input range: " <> show actual
    <> ">"
    <> show max
  show (InvalidCharCode code) = "Invalid char code: " <> show code
  show (ExceedsMaxStep max) = "Exceeds max step: " <> show max
  show (ProcessKilled err) = "Process killed: " <> show err

type Settings =
  { memorySize :: Int
  , chunkNum :: Int -- 1回のまとまりで処理する個数
  }

-- | Execute BrainfkAST
-- | callback function is called when update output
exec
  :: Settings
  -> String
  -> BrainfkAST
  -> Effect
       { getOutput :: Effect String
       , waitFinish :: Aff Unit
       , getRuntimeError :: Effect (Maybe ExecError)
       , stop :: Effect Unit
       }
exec { memorySize, chunkNum } input (BrainfkAST ast) = do
  memory <- toEffect AST.new --メモリー
  _ <- toEffect $ replicateM (memorySize - 1) $ AST.push 0 memory -- 0埋め
  pointer <- Ref.new 0 -- ポインタ
  inputIndex <- Ref.new 0 -- 入力インデックス
  log <- Ref.new "" -- ログ
  runTimeError <- Ref.new Nothing -- エラー
  chunk <- Ref.new 0
  isStop <- Ref.new false

  let
    writeError :: ExecError -> Effect Unit
    writeError err = Ref.write (Just err) runTimeError

    checkError :: Effect Boolean
    checkError = do
      err <- Ref.read runTimeError
      pure $ isJust err

    modifyReference :: (Int -> Int) -> Effect Unit
    modifyReference f = do
      pointer' <- Ref.read pointer
      res <- liftEffect $ toEffect $ AST.modify pointer' f memory
      when (not res) $ writeError $ OutOfMemoryRange (memorySize - 1) pointer'

    loop :: Statement -> Aff Unit
    loop statement = do
      case statement of
        StatementEnd -> pure unit
        StatementCont command statement' -> do
          case command of
            PointerIncrement i -> liftEffect $ do
              Ref.modify_ (_ + i) pointer
            ReferenceIncrement i -> liftEffect $ do
              modifyReference (_ + i)
            Input -> liftEffect $ do
              inputIndex' <- liftEffect $ Ref.read inputIndex
              case charAt inputIndex' input of
                Just c -> do
                  Ref.modify_ (_ + 1) inputIndex
                  modifyReference (const $ toCharCode c)
                Nothing -> writeError $ OutOfInputRange (length input - 1)
                  inputIndex'
            Output -> liftEffect $ do
              pointer' <- Ref.read pointer
              res <- toEffect $ AST.peek pointer' memory
              case res of
                Just i -> case fromCharCode i of
                  Just c -> do
                    Ref.modify_ (toString <<< snoc c) log
                  Nothing -> writeError $ InvalidCharCode i
                Nothing -> writeError $ OutOfMemoryRange (memorySize - 1)
                  pointer'
            Loop statement'' -> do
              let
                loop' = do
                  delayZero
                  pointer' <- liftEffect $ Ref.read pointer
                  res <- liftEffect $ toEffect $ AST.peek pointer' memory
                  when (res /= Just 0) do
                    loop statement''
                    checkError' <- liftEffect $ checkError
                    isStop' <- liftEffect $ Ref.read isStop
                    when (not checkError' && not isStop') $ loop'
              loop'
          chunk' <- liftEffect $ Ref.modify (_ + 1) chunk
          if chunk' >= chunkNum then do
            delayZero
            liftEffect $ Ref.write 0 chunk
            checkError' <- liftEffect $ checkError
            isStop' <- liftEffect $ Ref.read isStop
            when (not checkError' && not isStop') $ loop statement'
          else loop statement'
  fiber <- launchAff $ loop ast
  pure
    { getOutput: do
        log' <- Ref.read log
        Ref.write "" log
        pure log'
    , waitFinish: joinFiber fiber
    , getRuntimeError: Ref.read runTimeError
    , stop: Ref.write true isStop
    }
