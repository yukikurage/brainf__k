module Brainfk.System.Transpile
  ( CellSize(..)
  , Settings
  , Transpiled(..)
  , defaultSettings
  , transpile
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State, execState, get, modify_, put)
import Data.Foldable (fold)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String.CodeUnits (charAt)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (debugger, traceM)

newtype Transpiled = Transpiled String

derive newtype instance Eq Transpiled
derive newtype instance Ord Transpiled
derive newtype instance Show Transpiled
derive newtype instance Semigroup Transpiled
derive newtype instance Monoid Transpiled

data CellSize = Bit8 | Bit16 | Bit32

derive instance Eq CellSize
derive instance Ord CellSize

type Settings r =
  ( memorySize :: Int
  , cellSize :: CellSize
  | r
  )

defaultSettings :: Record (Settings ())
defaultSettings =
  { memorySize: 30000
  , cellSize: Bit8
  }

transpile :: forall r. Record (Settings r) -> String -> String -> Transpiled
transpile settings code input = Transpiled $
  tPrelude settings input
    <> tCode code
    <> tReturn

{-
p: Pointer,
m: Memory,
i: Input,
x: Input Index,
o: Output,
c: Chunk,
z: IsStop,
f: Async
w: Wait
-}
tPrelude :: forall r. Record (Settings r) -> String -> String
tPrelude { memorySize, cellSize } input =
  "let p=0;let m=new Uint" <> bit <> "Array(" <> show memorySize
    <> ");let i="
    <> show input
    <>
      ";let x=0;let f=postMessage;"
  where
  bit = case cellSize of
    Bit8 -> "8"
    Bit16 -> "16"
    Bit32 -> "32"

tInt :: Int -> String
tInt n = if n < 0 then show n else "+" <> show n

tCompute :: String -> Int -> String
tCompute left n = case n of
  0 -> ""
  _ -> left <> case n of
    1 -> "++;"
    (-1) -> "--;"
    x | x > 0 -> "+=" <> show x <> ";"
    x -> "-=" <> show (abs x) <> ";"

tMemory :: Int -> String
tMemory n =
  if n == 0 then "m[p]"
  else "m[p"
    <> tInt n
    <> "]"

-- | 値に対する操作を一般化
data Operation a = Add a | Set a

derive instance Eq a => Eq (Operation a)
derive instance Ord a => Ord (Operation a)

instance Show a => Show (Operation a) where
  show = case _ of
    Add n -> "Add " <> show n
    Set n -> "Set " <> show n

appendOp
  :: forall a_
   . Semiring a_
  => Operation a_
  -> Operation a_
  -> Operation a_
appendOp _ (Set m) = Set m
appendOp (Add n) (Add m) = Add $ n + m
appendOp (Set n) (Add m) = Set $ n + m

tOperation :: String -> Operation Int -> String
tOperation left = case _ of
  Add n -> tCompute left n
  Set x -> left <> "=" <> show x <> ";"

type TCodeState =
  { code :: String
  , pointer :: Int
  , position :: Int
  , stack :: Map Int (Operation Int)
  , transpiled :: String
  }

addTranspile :: String -> State TCodeState Unit
addTranspile str = modify_ \s -> s { transpiled = s.transpiled <> str }

applyStackI :: Int -> State TCodeState Unit
applyStackI i = do
  { stack } <- get
  case Map.lookup i stack of
    Nothing -> pure unit
    Just op -> do
      addTranspile $ tOperation (tMemory i) op
      modify_ \s -> s { stack = Map.delete i (s.stack) }

applyStack :: (Int -> Operation Int -> Boolean) -> State TCodeState Unit
applyStack f = do
  { stack } <- get
  modify_ \s -> s { stack = Map.filterWithKey (not f) stack }
  addTranspile (fold $ Map.mapMaybeWithKey g $ stack)
  where
  g k v =
    if f k v then
      Just $ tOperation (tMemory k) v
    else Nothing

applyStackAll :: State TCodeState Unit
applyStackAll = applyStack (const $ const true)

incrPointer :: Int -> State TCodeState Unit
incrPointer i = modify_ \s -> s { pointer = s.pointer + i }

incrPos :: State TCodeState Unit
incrPos = modify_ \s -> s { position = s.position + 1 }

resetPointer :: State TCodeState Unit
resetPointer = do
  { pointer } <- get
  applyStackAll
  addTranspile $ tCompute "p" pointer
  modify_ \s -> s { pointer = 0 }

tLoop :: State TCodeState Unit
tLoop = do
  s <- get
  let
    l = execState tCodeState
      { code: s.code
      , pointer: 0
      , position: s.position + 1
      , stack: Map.empty
      , transpiled: ""
      }
  case l of
    -- 最適化
    _
      | l.pointer == 0 && l.transpiled == "" && Map.lookup 0 l.stack == Just
          (Add (-1)) ->
          case Map.lookup s.pointer s.stack of
            -- 最適化1
            Just (Set 0) -> pure unit
            -- 最適化2
            Just (Set m) -> put $ s
              { stack = Map.insertWith appendOp s.pointer (Set 0)
                  $ Map.fromFoldable
                  $ map
                      ( \(k /\ v) -> (k + s.pointer) /\ case v of
                          Add v' -> Add $ v' * m
                          _ -> v
                      )
                      ( Map.toUnfoldable l.stack
                          :: Array (Int /\ Operation Int)
                      )
              }
            -- 最適化3
            _ -> do
              applyStackI s.pointer
              forWithIndex_ (Map.delete 0 l.stack) \i -> case _ of
                Set v -> do
                  addTranspile $ "if(" <> tMemory s.pointer <> "){"
                  addTranspile $ tMemory (s.pointer + i) <> "=" <> show v
                    <> ";"
                  addTranspile "}else{"
                  applyStackI (s.pointer + i)
                  addTranspile "}"
                Add v -> do
                  applyStackI (s.pointer + i)
                  addTranspile $ case v of
                    0 -> ""
                    1 -> tMemory (s.pointer + i) <> "+=" <> tMemory s.pointer <>
                      ";"
                    (-1) -> tMemory (s.pointer + i) <> "-=" <> tMemory s.pointer
                      <> ";"
                    _ | v > 0 -> tMemory (s.pointer + i) <> "+=" <> show v
                      <> "*"
                      <> tMemory s.pointer
                      <> ";"
                    _ | otherwise -> tMemory (s.pointer + i) <> "-="
                      <> show (abs v)
                      <> "*"
                      <> tMemory s.pointer
                      <> ";"
              modify_ \s' -> s'
                { stack = Map.insertWith appendOp s.pointer (Set 0) s'.stack }
    -- | 通常のループ
    _ -> do
      resetPointer
      addTranspile "while(m[p]){"
      let loopState = execState resetPointer l
      addTranspile loopState.transpiled
      addTranspile "}"
  modify_ $ _ { position = l.position }
  pure unit

tCodeState :: State TCodeState Unit
tCodeState = flip tailRecM unit \unit -> do
  s@{ position, code } <- get
  case charAt position code of
    Nothing -> pure (Done unit)
    Just ']' -> pure (Done unit)
    Just '[' -> tLoop *> incrPos *> pure (Loop unit)
    Just '>' -> incrPointer 1 *> incrPos *> pure (Loop unit)
    Just '<' -> incrPointer (-1) *> incrPos *> pure (Loop unit)
    Just '+' -> do
      put $ s { stack = Map.insertWith appendOp s.pointer (Add 1) s.stack }
      incrPos
      pure $ Loop unit
    Just '-' -> do
      put $ s { stack = Map.insertWith appendOp s.pointer (Add (-1)) s.stack }
      incrPos
      pure $ Loop unit
    Just '.' -> do
      applyStackI s.pointer
      addTranspile $ "f(" <> tMemory s.pointer <> ");"
      incrPos
      pure $ Loop unit
    Just ',' -> do
      addTranspile $ "if(x<i.length){"
        <> tMemory s.pointer
        <> "=i.codePointAt(x);x=x+1;}"
      incrPos
      pure $ Loop unit
    Just _ -> incrPos *> pure (Loop unit)

tCode :: String -> String
tCode code = (_.transpiled) $ execState tCodeState
  { code
  , pointer: 0
  , position: 0
  , stack: Map.empty
  , transpiled: ""
  }

tReturn :: String
tReturn =
  "f('f');"
