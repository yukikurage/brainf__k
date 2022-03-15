module Brainfk.System.Transpile where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)

type Settings r =
  ( memorySize :: Int
  , cellSize :: Int
  , chunkNum :: Int -- 1回のまとまりで処理する個数
  | r
  )

defaultSettings :: Record (Settings ())
defaultSettings =
  { memorySize: 512
  , chunkNum: 100
  , cellSize: 256
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
tPrelude { memorySize } input =
  "let p=0;let m=[... new Array(" <> show memorySize
    <> ")].fill(0);let i="
    <> show input
    <>
      ";let x=0;let o='';let c=0;"

-- pointerPos に現在のポインターの位置をもちまわす
-- while 文の最後にずらして補正
tStatement
  :: forall r. Record (Settings r) -> Statement -> Int -> String
tStatement { memorySize } (StatementEnd) pointerPos =
  if pointerPos == 0 then ""
  else "p=(p+" <> show pointerPos
    <> ")%"
    <> show memorySize
    <> ";"
tStatement
  settings@{ memorySize, cellSize, chunkNum }
  (StatementCont command statement)
  pointerPos =
  let
    mkMemoryAcc =
      if pointerPos == 0 then "m[p]"
      else "m[(p+"
        <> show pointerPos
        <> ")%"
        <> show memorySize
        <> "]"
  in
    case command of
      PointerIncrement _ n -> tStatement settings statement
        ((pointerPos + n) `mod` memorySize)
      PointerDecrement _ n -> tStatement settings statement
        ((pointerPos - n) `mod` memorySize)
      ReferenceIncrement _ n -> mkMemoryAcc <> "=(" <> mkMemoryAcc <> "+"
        <> show n
        <> ")%"
        <> show cellSize
        <> ";"
        <>
          tStatement settings statement pointerPos
      ReferenceDecrement _ n -> mkMemoryAcc <> "=(" <> mkMemoryAcc <> "+"
        <> show ((-n) `mod` cellSize)
        <> ")%"
        <> show cellSize
        <> ";"
        <>
          tStatement settings statement pointerPos
      Input _ -> "if(x>=i.length){throw new Error('Exceeds Input Range')};"
        <> mkMemoryAcc
        <> "=i.codePointAt(x);x++;"
        <> tStatement settings statement pointerPos

      Output _ -> "o+=String.fromCodePoint("
        <> mkMemoryAcc
        <> ");"
        <> "if(o.length>="
        <> show chunkNum
        <> "){postMessage({type:'output',value:o});o=''};"
        <> tStatement settings statement pointerPos
      Loop _ loopStatement ->
        ( if pointerPos == 0 then ""
          else "p=(p+" <> show pointerPos <> ")%"
            <> show memorySize
            <> ";"
        )
          <>
            "while(m[p]){"
          <> tStatement settings loopStatement 0
          <> "};"
          <> tStatement settings statement 0

tReturn :: String
tReturn =
  "postMessage({type:'output',value:o});postMessage({type:'finish'})"
