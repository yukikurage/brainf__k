module Brainfk.System.Transpile where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (fold)
import Data.Foldable (all)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (abs)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple.Nested (type (/\), (/\))

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

tokens :: Array Char
tokens = [ '>', '<', '+', '-', '.', ',', '[', ']' ]

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
      ";let x=0;"
  where
  bit = case cellSize of
    Bit8 -> "8"
    Bit16 -> "16"
    Bit32 -> "32"

showNeg :: Int -> String
showNeg n = if n < 0 then show n else "+" <> show n

showNegCompute :: String -> Int -> String
showNegCompute left n = case n of
  0 -> ""
  _ -> left <> case n of
    1 -> "++;"
    (-1) -> "--;"
    x | x > 0 -> "+=" <> show x <> ";"
    x -> "-=" <> show (abs x) <> ";"

mkMemoryAcc' :: Int -> String
mkMemoryAcc' n =
  if n == 0 then "m[p]"
  else "m[p"
    <> showNeg n
    <> "]"

type TCodeState =
  { pointer :: Int
  , position :: Int
  , stacked :: Map Int Operation
  , transpiled :: String
  }

data Operation = Add Int | Set Int

isAdd :: Operation -> Boolean
isAdd (Add _) = true
isAdd _ = false

isSet :: Operation -> Boolean
isSet (Set _) = true
isSet _ = false

derive instance Eq Operation
derive instance Ord Operation

compositeOperation :: Operation -> Operation -> Operation
compositeOperation _ (Set m) = Set m
compositeOperation (Add n) (Add m) = Add (n + m)
compositeOperation (Set n) (Add m) = Set $ n + m

tCode :: String -> String
tCode code = (_.transpiled) $ applyStack $ go
  { pointer: 0
  , position: 0
  , stacked: Map.empty
  , transpiled: ""
  }
  where
  -- | 現在の位置の Char を取得
  token :: TCodeState -> Maybe Char
  token { position } = CodeUnits.charAt position code

  -- | コードの現在位置を1すすめる
  incr :: TCodeState -> TCodeState
  incr state@{ position } = state { position = position + 1 }

  -- | メモリの位置のトランスパイル
  tMemory :: Int -> String
  tMemory diff =
    if diff == 0 then "m[p]"
    else "m[p"
      <> showNeg diff
      <> "]"

  -- | pointer の位置をトランスパイラの内部位置にセット
  -- | 不整合が起こるので内部的に applyStack を使っている
  setPointer :: TCodeState -> TCodeState
  setPointer state =
    let
      state'@{ pointer, transpiled } = applyStack state
    in
      state'
        { pointer = 0
        , transpiled = transpiled <> showNegCompute "p" pointer
        }

  -- | stack にたまった値を取り出す
  applyStack :: TCodeState -> TCodeState
  applyStack state@{ stacked, transpiled } = state
    { stacked = Map.empty
    , transpiled = transpiled <> fold (map f $ Map.toUnfoldable stacked)
    }
    where
    f :: Int /\ Operation -> String
    f (n /\ (Add m)) = showNegCompute (tMemory n) m
    f (n /\ (Set m)) = tMemory n <> "=" <> show m <> ";"

  go state = tailRec goTailRec state

  -- | pointer: ポインタ位置
  -- | position: コードの位置
  -- | stacked: 変数の操作のスタック．エフェクト (.,[]) が発生したら transpiled に適用し空にする
  -- | transpiled: トランスパイルされたコード
  goTailRec :: TCodeState -> Step TCodeState TCodeState
  goTailRec state = case token state of
    Nothing -> Done $ incr $ state
    Just ']' -> Done $ incr $ state
    Just '[' ->
      let
        internalLoop = go
          { pointer: 0
          , position: state.position + 1
          , stacked: Map.empty
          , transpiled: ""
          }
      in
        Loop $ case internalLoop of
          --ループ最適化1
          { transpiled: "", pointer: 0, stacked }
            | Map.lookup 0 stacked == Just (Add (-1))
                && all isSet
                  (Map.delete 0 stacked) ->
                state
                  { position = internalLoop.position
                  , stacked =
                      Map.insertWith compositeOperation state.pointer (Set 0)
                        $ Map.unionWith compositeOperation state.stacked
                        $ Map.fromFoldable
                        $ map (\(k /\ v) -> ((k + state.pointer) /\ v))
                        $
                          ( Map.toUnfoldable $ Map.delete 0 stacked
                              :: Array (Int /\ Operation)
                          )
                  }
          -- ループ最適化2
          { transpiled: "", pointer: 0, stacked }
            | Map.lookup 0 stacked == Just (Add (-1)) &&
                maybe false isSet (Map.lookup state.pointer state.stacked) ->
                let
                  v0' = Map.lookup state.pointer state.stacked
                  v0 = case v0' of
                    Just (Set x) -> x
                    _ -> 0
                in
                  state
                    { position = internalLoop.position
                    , stacked =
                        Map.insertWith compositeOperation state.pointer (Set 0)
                          $ Map.unionWith compositeOperation state.stacked
                          $ Map.fromFoldable
                          $ map
                              ( \(k /\ v) -> (k + state.pointer) /\ case v of
                                  Set x -> Set x
                                  Add x -> Add $ x * v0
                              )
                          $
                            ( Map.toUnfoldable $ Map.delete 0 stacked
                                :: Array (Int /\ Operation)
                            )
                    }

          -- ループ最適化3
          { transpiled: "", pointer: 0, stacked }
            | Map.lookup 0 stacked == Just (Add (-1)) ->
                beforeLoop
                  { position = internalLoop.position
                  , transpiled = beforeLoop.transpiled
                      <> fold
                        (map f $ Map.toUnfoldable $ Map.delete 0 $ stacked)
                  , stacked = Map.singleton beforeLoop.pointer (Set 0)
                  }
                where
                beforeLoop = applyStack state
                f (n /\ (Add m)) = case m of
                  0 -> ""
                  _ -> tMemory (n + beforeLoop.pointer) <> case m of
                    1 -> "+="
                      <> tMemory beforeLoop.pointer
                      <> ";"
                    (-1) -> "-=" <> tMemory beforeLoop.pointer
                      <> ";"
                    x | x > 0 -> "+=" <> show x
                      <> "*"
                      <> tMemory beforeLoop.pointer
                      <> ";"
                    x -> "-="
                      <> show (abs x)
                      <> "*"
                      <> tMemory beforeLoop.pointer
                      <>
                        ";"
                f (n /\ (Set m)) = "if(" <> tMemory beforeLoop.pointer <> "){"
                  <> tMemory (n + beforeLoop.pointer)
                  <> "="
                  <> show m
                  <> ";}"
          -- それ以外
          _ ->
            let
              appliedInternalLoop = setPointer internalLoop
              beforeLoop = setPointer state
            in
              beforeLoop
                { position = appliedInternalLoop.position
                , transpiled = beforeLoop.transpiled <> "while(m[p]){"
                    <> appliedInternalLoop.transpiled
                    <> "}"
                }
    Just '>' -> Loop $ incr $ state { pointer = state.pointer + 1 }
    Just '<' -> Loop $ incr $ state { pointer = state.pointer - 1 }
    Just '+' -> Loop $ incr $ state
      { stacked = Map.insertWith compositeOperation state.pointer (Add 1)
          state.stacked
      }
    Just '-' -> Loop $ incr $ state
      { stacked = Map.insertWith compositeOperation state.pointer (Add $ -1)
          state.stacked
      }
    Just '.' ->
      let
        prev = applyStack state
      in
        Loop $ incr
          $ prev
              { transpiled = prev.transpiled <> "postMessage("
                  <> tMemory prev.pointer
                  <>
                    ");"
              }
    Just '.' ->
      let
        prev = applyStack state
      in
        Loop $ incr
          $ prev
              { transpiled = prev.transpiled <> "if(x<i.length){"
                  <> tMemory prev.pointer
                  <> "=i.codePointAt(x);x=x+1;}"
              }
    _ -> Loop $ incr state

tReturn :: String
tReturn =
  "postMessage('f');"
