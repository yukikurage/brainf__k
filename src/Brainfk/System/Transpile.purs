module Brainfk.System.Transpile where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Control.Promise (Promise, toAff)
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Class.Console (logShow)

data CellSize = Bit8 | Bit16 | Bit32

derive instance Eq CellSize

type Settings r =
  ( memorySize :: Int
  , cellSize :: CellSize
  , chunkNum :: Int -- 1回のまとまりで処理する個数
  | r
  )

defaultSettings :: Record (Settings ())
defaultSettings =
  { memorySize: 2048
  , chunkNum: 20
  , cellSize: Bit8
  }

foreign import exec_
  :: forall a
   . (a -> Maybe a)
  -> Maybe a
  -> String
  -> Effect
       { getOutput :: Effect String
       , stop :: Effect Unit
       , waitFinish :: Promise (Maybe Error)
       }

exec
  :: forall r
   . Record (Settings r)
  -> BrainfkAST
  -> String
  -> Effect
       { getOutput :: Effect String
       , stop :: Effect Unit
       , waitFinish :: Aff (Maybe Error)
       }
exec settings brainfkAST input = do
  let
    transpiled = transpile settings brainfkAST input
  res@{ waitFinish } <- exec_ Just Nothing
    transpiled
  logShow brainfkAST
  pure res { waitFinish = toAff waitFinish }

transpile :: forall r. Record (Settings r) -> BrainfkAST -> String -> String
transpile settings (BrainfkAST statement) input = tPrelude settings input
  <> tStatement
    settings
    statement
    0
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
    <> ").fill(0);let i="
    <> show input
    <>
      ";let x=0;let c=0;"
  where
  bit = case cellSize of
    Bit8 -> "8"
    Bit16 -> "16"
    Bit32 -> "32"

-- pointerPos に現在のポインターの位置をもちまわす
-- while 文の最後にずらして補正
tStatement
  :: forall r. Record (Settings r) -> Statement -> Int -> String
tStatement settings (Statement commands) pointerPos = case uncons commands of
  Nothing ->
    if pointerPos == 0 then ""
    else "p+=" <> show pointerPos
      <> ";"
  Just { head: command, tail } ->
    let
      statement = Statement tail
      showNeg n = if n < 0 then show n else "+" <> show n
      mkMemoryAcc =
        if pointerPos == 0 then "m[p]"
        else "m[p"
          <> showNeg pointerPos
          <> "]"
      mkMemoryAcc' n =
        if n == 0 then "m[p]"
        else "m[p"
          <> showNeg n
          <> "]"
    in
      case command of
        -- memory clear optimizing
        Loop _ (Statement [ ReferenceIncrement _ (-1) ]) ->
          mkMemoryAcc <> "=0;" <> tStatement settings statement pointerPos
        -- memory swap optimizing
        Loop _
          ( Statement
              [ ReferenceIncrement _ (-1)
              , PointerIncrement _ n
              , ReferenceIncrement _ x
              , PointerIncrement _ m
              ]
          ) | n + m == 0 -> mkMemoryAcc' (pointerPos + n) <> "+="
          <> (if x == 1 then "" else show x <> "*")
          <> mkMemoryAcc
          <> ";"
          <> mkMemoryAcc
          <> "=0;"
          <> tStatement settings statement pointerPos
        Loop _
          ( Statement
              [ PointerIncrement _ n
              , ReferenceIncrement _ x
              , PointerIncrement _ m
              , ReferenceIncrement _ (-1)
              ]
          ) | n + m == 0 -> mkMemoryAcc' (pointerPos + n) <> "+="
          <> (if x == 1 then "" else show x <> "*")
          <> mkMemoryAcc
          <> ";"
          <> mkMemoryAcc
          <> "=0;"
          <> tStatement settings statement pointerPos
        PointerIncrement _ n -> tStatement settings statement $ pointerPos + n
        ReferenceIncrement _ n -> mkMemoryAcc <> "+="
          <> show n
          <> ";"
          <> tStatement settings statement pointerPos
        Input _ ->
          "if(x>=i.length){throw new Error('Exceeds Input Range');};"
            <> mkMemoryAcc
            <> "=i.codePointAt(x);x++;"
            <> tStatement settings statement pointerPos
        Output _ -> "postMessage(String.fromCodePoint("
          <> mkMemoryAcc
          <> "));"
          <> tStatement settings statement pointerPos
        Loop _ loopStatement ->
          ( if pointerPos == 0 then ""
            else "p+=" <> show pointerPos <> ";"
          )
            <>
              "while(m[p]){"
            <> tStatement settings loopStatement 0
            <> "};"
            <> tStatement settings statement 0

tReturn :: String
tReturn =
  "postMessage('fi')"
