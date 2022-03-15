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
  , chunkNum: 20
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
  "let p=0;let m=new Uint16Array(" <> show memorySize
    <> ");let i="
    <> show input
    <>
      ";let x=0;let c=0;"

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
  settings@{ memorySize, cellSize }
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
    mkMemoryAcc' n =
      if n == 0 then "m[p]"
      else "m[(p+"
        <> show n
        <> ")%"
        <> show memorySize
        <> "]"
  in
    case command of
      -- memory clear optimizing
      Loop _ (StatementCont (ReferenceIncrement _ (-1)) StatementEnd) ->
        mkMemoryAcc <> "=0;" <> tStatement settings statement pointerPos
      -- memory swap optimizing
      Loop _
        ( StatementCont (ReferenceIncrement _ (-1))
            ( StatementCont (PointerIncrement _ n)
                ( StatementCont (ReferenceIncrement _ x)
                    (StatementCont (PointerIncrement _ m) StatementEnd)
                )
            )
        ) | n + m == 0 -> mkMemoryAcc' (pointerPos + n) <> "+="
        <> (if x == 1 then "" else show x <> "*")
        <> mkMemoryAcc
        <> ";"
        <> mkMemoryAcc
        <> "=0;"
        <> tStatement settings statement pointerPos
      Loop _
        ( StatementCont (PointerIncrement _ n)
            ( StatementCont (ReferenceIncrement _ x)
                ( StatementCont (PointerIncrement _ m)
                    (StatementCont (ReferenceIncrement _ (-1)) StatementEnd)
                )
            )
        ) | n + m == 0 -> mkMemoryAcc' (pointerPos + n) <> "+="
        <> (if x == 1 then "" else show x <> "*")
        <> mkMemoryAcc
        <> ";"
        <> mkMemoryAcc
        <> "=0;"
        <> tStatement settings statement pointerPos
      Loop _
        ( StatementCont (ReferenceIncrement _ x)
            ( StatementCont (PointerIncrement _ n)
                ( StatementCont (ReferenceIncrement _ (-1))
                    (StatementCont (PointerIncrement _ m) StatementEnd)
                )
            )
        ) | n + m == 0 -> mkMemoryAcc <> "+="
        <> (if x == 1 then "" else show x <> "*")
        <> mkMemoryAcc' (pointerPos + n)
        <> ";"
        <> mkMemoryAcc' (pointerPos + n)
        <> "=0;"
        <> tStatement settings statement pointerPos
      Loop _
        ( StatementCont (PointerIncrement _ n)
            ( StatementCont (ReferenceIncrement _ (-1))
                ( StatementCont (PointerIncrement _ m)
                    (StatementCont (ReferenceIncrement _ x) StatementEnd)
                )
            )
        ) | n + m == 0 -> mkMemoryAcc <> "+="
        <> (if x == 1 then "" else show x <> "*")
        <> mkMemoryAcc' (pointerPos + n)
        <> ";"
        <> mkMemoryAcc' (pointerPos + n)
        <> "=0"
        <> tStatement settings statement pointerPos
      PointerIncrement _ n -> tStatement settings statement
        ((pointerPos + n) `mod` memorySize)
      ReferenceIncrement _ n -> mkMemoryAcc <> "=(" <> mkMemoryAcc <> "+"
        <> show ((cellSize + n) `mod` cellSize)
        <> ")%"
        <> show cellSize
        <> ";"
        <>
          tStatement settings statement pointerPos
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
  "postMessage('fi')"
