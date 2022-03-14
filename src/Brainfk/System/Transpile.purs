module Brainfk.System.Transpile where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Brainfk.Util (setZeroTimeout)
import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)

type Settings r =
  ( memorySize :: Int
  , cellSize :: Int
  , chunkNum :: Int -- 1回のまとまりで処理する個数
  | r
  )

defaultSettings :: Record (Settings ())
defaultSettings =
  { memorySize: 256
  , chunkNum: 10000
  , cellSize: 256
  }

foreign import exec_
  :: Effect Unit
  -> (Effect Unit -> Effect Unit)
  -> String
  -> Effect
       { getOutput :: Effect String
       , getStep :: Effect Int
       , stop :: Effect Unit
       , getMemory :: Effect (Array Int)
       , waitFinish :: Promise Unit
       }

exec
  :: forall r
   . Record (Settings r)
  -> BrainfkAST
  -> String
  -> Effect
       { getMemory :: Effect (Array Int)
       , getOutput :: Effect String
       , getStep :: Effect Int
       , stop :: Effect Unit
       , waitFinish :: Aff Unit
       }
exec settings brainfkAST input = do
  let
    transpiled = transpile settings brainfkAST input
  log transpiled
  res@{ waitFinish } <- exec_ (pure unit)
    setZeroTimeout
    transpiled
  pure res { waitFinish = toAff waitFinish }

transpile :: forall r. Record (Settings r) -> BrainfkAST -> String -> String
transpile settings (BrainfkAST statement) input = tPrelude settings input
  <> tStatement
    settings
    statement
    true
  <> tReturn

tPrelude :: forall r. Record (Settings r) -> String -> String
tPrelude { memorySize } input =
  """
"use strict";
let pointer=0;
const memory=[...Array(""" <> show memorySize
    <>
      """)].map(() => 0);
let input='"""
    <> show input
    <>
      """';
let inputIndex=0;
let output='';
let isStop=false;
let step=0;
let chunk=0;
const mod=(a,b)=>(a%b+b)%b;
const setZeroTimeoutPromise = () => new Promise((resolve) => {
  setZeroTimeout(resolve)();
});

const wait = (async () => {
"""

tStatement :: forall r. Record (Settings r) -> Statement -> Boolean -> String
tStatement _ (StatementEnd) true = "callback();"
tStatement _ (StatementEnd) false = ""
tStatement
  settings@{ memorySize, cellSize, chunkNum }
  (StatementCont command statement)
  isGlobal =
  case command of
    PointerIncrement _ n -> "pointer=mod(pointer+" <> show n <> ","
      <> show memorySize
      <> ");step+="
      <> show n
      <> ";chunk+=1;"
      <> tStatement settings statement isGlobal
    PointerDecrement _ n -> "pointer=mod(pointer-" <> show n <> ","
      <> show memorySize
      <> ");step+="
      <> show n
      <> ";chunk+=1;"
      <> tStatement settings statement isGlobal
    ReferenceIncrement _ n ->
      "memory[pointer]=mod(memory[pointer]+" <> show n <> "," <> show cellSize
        <> ");step+="
        <> show n
        <> ";chunk+=1;"
        <> tStatement settings statement isGlobal
    ReferenceDecrement _ n ->
      "memory[pointer]=mod(memory[pointer]-" <> show n <> "," <> show cellSize
        <> ");step+="
        <> show n
        <> ";chunk+=1;"
        <> tStatement settings statement isGlobal
    Input _ ->
      "memory[pointer]=input.codePointAt(inputIndex);inputIndex++;step+=1;chunk+=1;"
        <> tStatement settings statement isGlobal
    Output _ ->
      "output+=String.fromCodePoint(memory[pointer]);step+=1;chunk+=1;"
        <> tStatement settings statement isGlobal
    Loop _ loopStatement ->
      "while(memory[pointer]!==0){if(chunk>=" <> show chunkNum
        <> "){chunk=0;await setZeroTimeoutPromise();};"
        <> tStatement settings loopStatement false
        <> "};"
        <> tStatement settings statement isGlobal

tReturn :: String
tReturn =
  """
})();
return {
  getMemory: ()=>[...memory],
  getOutput: ()=>{
    const res=output;
    output="";
    return res;
  },
  getStep: ()=>step,
  stop: ()=>{
    isStop=true;
  },
  waitFinish: wait
}"""
