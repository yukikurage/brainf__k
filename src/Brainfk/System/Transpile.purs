module Brainfk.System.Transpile where

import Prelude

import Brainfk.System.Data.BrainfkAST (BrainfkAST(..), Command(..), Statement(..))
import Brainfk.Util (setZeroTimeout)
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
  { memorySize: 256
  , chunkNum: 200000
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
       , getStep :: Effect Int
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
       , getStep :: Effect Int
       , stop :: Effect Unit
       , waitFinish :: Aff (Maybe Error)
       }
exec settings brainfkAST input = do
  let
    transpiled = transpile settings brainfkAST input
  res@{ waitFinish } <- exec_ Just Nothing
    setZeroTimeout
    transpiled
  pure res { waitFinish = toAff waitFinish }

transpile :: forall r. Record (Settings r) -> BrainfkAST -> String -> String
transpile settings (BrainfkAST statement) input = tPrelude settings input
  <> tStatement
    settings
    statement
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

tStatement :: forall r. Record (Settings r) -> Statement -> String
tStatement _ (StatementEnd) = ""
tStatement
  settings@{ memorySize, cellSize, chunkNum }
  (StatementCont command statement) =
  case command of
    PointerIncrement _ n -> "pointer=mod(pointer+" <> show n <> ","
      <> show memorySize
      <> ");step+="
      <> show n
      <> ";chunk+=1;"
      <> tStatement settings statement
    PointerDecrement _ n -> "pointer=mod(pointer-" <> show n <> ","
      <> show memorySize
      <> ");step+="
      <> show n
      <> ";chunk+=1;"
      <> tStatement settings statement
    ReferenceIncrement _ n ->
      "memory[pointer]=mod(memory[pointer]+" <> show n <> "," <> show cellSize
        <> ");step+="
        <> show n
        <> ";chunk+=1;"
        <> tStatement settings statement
    ReferenceDecrement _ n ->
      "memory[pointer]=mod(memory[pointer]-" <> show n <> "," <> show cellSize
        <> ");step+="
        <> show n
        <> ";chunk+=1;"
        <> tStatement settings statement
    Input _ ->
      "if(inputIndex>=input.length){throw new Error('Exceeds Input Range')};memory[pointer]=input.codePointAt(inputIndex);inputIndex++;step+=1;chunk+=1;"
        <> tStatement settings statement
    Output _ ->
      "output+=String.fromCodePoint(memory[pointer]);step+=1;chunk+=1;"
        <> tStatement settings statement
    Loop _ loopStatement ->
      "while(memory[pointer]!==0){chunk+=1;if(chunk>=" <> show chunkNum
        <>
          "){chunk=0;if(isStop){throw new Error('Process Killed')};await setZeroTimeoutPromise();};"
        <> tStatement settings loopStatement
        <> "};"
        <> tStatement settings statement

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
