module Brainfk.System.Transpile where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Brainfk.Util (setZeroTimeout)
import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Class.Console (log)

type Settings r =
  ( memorySize :: Int
  , cellSize :: Int
  , chunkNum :: Int -- 1回のまとまりで処理する個数
  | r
  )

defaultSettings :: Record (Settings ())
defaultSettings =
  { memorySize: 512
  , chunkNum: 300000
  , cellSize: 256
  }

foreign import exec_
  :: forall a
   . (a -> Maybe a)
  -> Maybe a
  -> (Effect Unit -> Effect Unit)
  -> String
  -> Effect
       { getOutput :: Effect String
       , stop :: Effect Unit
       , getMemory :: Effect (Array Int)
       , waitFinish :: Promise (Maybe Error)
       }

exec
  :: forall r
   . Record (Settings r)
  -> BrainfkAST
  -> String
  -> Effect
       { getMemory :: Effect (Array Int)
       , getOutput :: Effect String
       , stop :: Effect Unit
       , waitFinish :: Aff (Maybe Error)
       }
exec settings brainfkAST input = do
  let
    transpiled = transpile settings brainfkAST input
  log transpiled
  res@{ waitFinish } <- exec_ Just Nothing
    setZeroTimeout
    transpiled
  pure res { waitFinish = toAff waitFinish }

transpile :: forall r. Record (Settings r) -> BrainfkAST -> String -> String
transpile settings (BrainfkAST statement) input = tPrelude settings input
  <> tStatement
    settings
    statement
    0
  <> tReturn

tPrelude :: forall r. Record (Settings r) -> String -> String
tPrelude { memorySize } input =
  "'use strict';let p=0;let m=[... new Array(" <> show memorySize
    <> ")].fill(0);let i="
    <> show input
    <>
      ";let x=0;let o='';let z=false;let c=0;let f=async ()=>{c=0;if(z){throw new Error('Process Killed')};await new Promise((v)=>{setZeroTimeout(v)();});};let w=(async ()=>{"

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
      PointerIncrement _ n -> tStatement settings statement (pointerPos + n)
      PointerDecrement _ n -> tStatement settings statement
        ((pointerPos - n) `mod` memorySize)
      ReferenceIncrement _ n -> mkMemoryAcc <> "=(" <> mkMemoryAcc <> "+"
        <> show n
        <> ")%"
        <> show cellSize
        <> ";c++;"
        <>
          tStatement settings statement pointerPos
      ReferenceDecrement _ n -> mkMemoryAcc <> "=(" <> mkMemoryAcc <> "+"
        <> show ((-n) `mod` cellSize)
        <> ")%"
        <> show cellSize
        <> ";c++;"
        <>
          tStatement settings statement pointerPos
      Input _ -> "if(x>=i.length){throw new Error('Exceeds Input Range')};"
        <> mkMemoryAcc
        <> "=i.codePointAt(x);x++;"
        <> tStatement settings statement pointerPos

      Output _ -> "o+=String.fromCodePoint("
        <> mkMemoryAcc
        <> ");"
        <> tStatement settings statement pointerPos
      Loop _ loopStatement ->
        ( if pointerPos == 0 then ""
          else "p=(p+" <> show pointerPos <> ")%"
            <> show memorySize
            <> ";"
        )
          <>
            "while(m[p]){c++;if(c>="
          <> show chunkNum
          <> "){await f();};"
          <> tStatement settings loopStatement 0
          <> "};"
          <> tStatement settings statement 0

tReturn :: String
tReturn =
  "})();return {getMemory:()=>[...m],getOutput:()=>{let r=o;o='';return r;},stop:()=>{z=true;},waitFinish: w}"
