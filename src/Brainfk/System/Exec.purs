module Brainfk.System.Exec where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.Rec.Class (whileJust)
import Control.Monad.ST.Global (toEffect)
import Data.Array (fromFoldable)
import Data.Array.ST as AST
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt, fromCharArray, length)
import Effect (Effect)
import Effect.Aff (Aff, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref

-- | OutOfRange max actual
data ExecError = OutOfMemoryRange Int Int | OutOfInputRange Int Int | InvalidCharCode Int

instance Show ExecError where
  show (OutOfMemoryRange max actual) = "Out of memory range: " <> show actual <> ">" <> show max
  show (OutOfInputRange max actual) = "Out of input range: " <> show actual <> ">" <> show max
  show (InvalidCharCode code) = "Invalid char code: " <> show code

type Settings =
  { memorySize :: Int
  }

-- | Execute BrainfkAST
-- | callback function is called when update output
exec
  :: Settings
  -> String
  -> BrainfkAST
  -> Effect { output :: Effect String, result :: Aff (Maybe ExecError) }
exec { memorySize } input (BrainfkAST ast) = do
  memory <- toEffect AST.new --メモリー
  _ <- toEffect $ replicateM (memorySize - 1) $ AST.push 0 memory -- 0埋め
  pointer <- Ref.new 0 -- ポインタ
  inputIndex <- Ref.new 0 -- 入力インデックス
  log <- Ref.new Nil -- ログ
  let
    modifyReference :: (Int -> Int) -> ExceptT ExecError Effect Unit
    modifyReference f = do
      pointer' <- lift $ Ref.read pointer
      res <- lift $ toEffect $ AST.modify pointer' f memory
      when (not res) $ throwError $ OutOfMemoryRange (memorySize - 1) pointer'

    loop :: Statement -> ExceptT ExecError Effect Unit
    loop = case _ of
      StatementEnd -> pure unit
      StatementCont command statement -> do
        case command of
          PointerIncrement i -> do
            _ <- lift $ Ref.modify (_ + i) pointer
            loop statement
          PointerDecrement i -> do
            _ <- lift $ Ref.modify (_ - i) pointer
            loop statement
          ReferenceIncrement i -> do
            modifyReference (_ + i)
            loop statement
          ReferenceDecrement i -> do
            modifyReference (_ - i)
            loop statement
          Input -> do
            inputIndex' <- lift $ Ref.read inputIndex
            case charAt inputIndex' input of
              Just c -> do
                modifyReference (const $ toCharCode c)
                loop statement
              Nothing -> throwError $ OutOfInputRange (length input - 1) inputIndex'
          Output -> do
            pointer' <- lift $ Ref.read pointer
            res <- lift $ toEffect $ AST.peek pointer' memory
            case res of
              Just i -> case fromCharCode i of
                Just c -> do
                  _ <- lift $ Ref.modify (c : _) log
                  loop statement
                Nothing -> throwError $ InvalidCharCode i
              Nothing -> throwError $ OutOfMemoryRange (memorySize - 1) pointer'
          Loop statement' -> do
            _ <- whileJust do
              pointer' <- lift $ Ref.read pointer
              res <- lift $ toEffect $ AST.peek pointer' memory
              lift $ Console.log $ show res
              if res /= Just 0 then Just <$> loop statement' else pure Nothing
            loop statement
  fiber <- launchAff $ liftEffect $ runExceptT $ loop ast
  pure
    { output: do
        log' <- Ref.read log
        pure $ fromCharArray $ fromFoldable log'
    , result: do
        res <- joinFiber fiber
        case res of
          Right _ -> pure Nothing
          Left e -> pure $ Just e
    }
