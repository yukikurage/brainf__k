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
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt, fromCharArray, length)
import Effect (Effect)
import Effect.Aff (Aff, Error, catchError, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
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
    <> " ∉ [0, "
    <> show max
    <> "]"
  show (OutOfInputRange max actual) = "Out of input range: " <> show actual
    <> " ∉ [0, "
    <> show max
    <> "]"
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
       , getStep :: Effect Int
       , waitFinish :: Aff (Maybe Error)
       , stop :: Effect Unit
       , getMemory :: Effect (Array Int)
       }
exec { memorySize, chunkNum } input (BrainfkAST ast) = do
  memory <- toEffect AST.new --メモリー
  _ <- toEffect $ replicateM (memorySize - 1) $ AST.push 0 memory -- 0埋め
  pointer <- Ref.new 0 -- ポインタ
  inputIndex <- Ref.new 0 -- 入力インデックス
  log <- Ref.new Nil -- ログ
  chunk <- Ref.new 0
  isStop <- Ref.new false
  step <- Ref.new 0

  let
    writeError :: ExecError -> Effect Unit
    writeError err = throw $ show err

    incrStep :: Int -> Effect Unit
    incrStep i = Ref.modify_ (_ + i) step

    modifyReference :: (Int -> Int) -> Effect Unit
    modifyReference f = do
      pointer' <- Ref.read pointer
      res <- liftEffect $ toEffect $ AST.modify pointer' f memory
      if not res then writeError $ OutOfMemoryRange (memorySize - 1) pointer'
      else pure unit

    loop :: Statement -> Aff Unit
    loop statement = do
      chunk' <- liftEffect $ Ref.modify (_ + 1) chunk
      if chunk' >= chunkNum then do
        liftEffect $ Ref.write 0 chunk
        delayZero
      else pure unit
      case statement of
        StatementCont command statement' -> do
          case command of
            ReferenceIncrement _ i -> liftEffect $ do
              incrStep i
              modifyReference (_ + i)
            PointerIncrement _ i -> liftEffect $ do
              incrStep i
              Ref.modify_ (_ + i) pointer
            PointerDecrement _ i -> liftEffect $ do
              incrStep i
              Ref.modify_ (_ - i) pointer
            Loop _ statement'' -> do
              let
                loop' = do
                  pointer' <- liftEffect $ Ref.read pointer
                  res <- liftEffect $ toEffect $ AST.peek pointer' memory
                  if res /= Just 0 then do
                    loop statement''
                    isStop' <- liftEffect $ Ref.read isStop
                    if not isStop' then loop' else pure unit
                  else pure unit
              loop'
            ReferenceDecrement _ i -> liftEffect $ do
              incrStep i
              modifyReference (_ - i)
            Output _ -> liftEffect $ do
              incrStep 1
              pointer' <- Ref.read pointer
              res <- toEffect $ AST.peek pointer' memory
              case res of
                Just i -> case fromCharCode i of
                  Just c -> do
                    Ref.modify_ (c : _) log
                  Nothing -> writeError $ InvalidCharCode i
                Nothing -> writeError $ OutOfMemoryRange (memorySize - 1)
                  pointer'
            Input _ -> liftEffect $ do
              incrStep 1
              inputIndex' <- liftEffect $ Ref.read inputIndex
              case charAt inputIndex' input of
                Just c -> do
                  Ref.modify_ (_ + 1) inputIndex
                  modifyReference (const $ toCharCode c)
                Nothing -> writeError $ OutOfInputRange (length input - 1)
                  inputIndex'
          loop statement'
        StatementEnd -> pure unit
  fiber <- launchAff $ loop ast
  pure
    { getOutput: do
        log' <- Ref.read log
        Ref.write Nil log
        pure $ fromCharArray $ reverse $ fromFoldable $ log'
    , getStep: Ref.read step
    , waitFinish: catchError (joinFiber fiber $> Nothing) (Just >>> pure)
    , stop: Ref.write true isStop
    , getMemory: toEffect $ AST.freeze memory
    }
