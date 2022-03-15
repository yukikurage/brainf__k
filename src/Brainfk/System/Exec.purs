module Brainfk.System.Exec where

import Prelude

import Effect.Aff (Error)

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

type Settings r =
  ( memorySize :: Int
  , cellSize :: Int
  , chunkNum :: Int -- 1回のまとまりで処理する個数
  , isLoopMemory :: Boolean --メモリの左端(あるいは右端)に到達したときにループするかどうか
  , isLoopCell :: Boolean -- メモリのセルがオーバーフローしたときにループするかどうか
  | r
  )

defaultSettings :: Record (Settings ())
defaultSettings =
  { memorySize: 256
  , chunkNum: 5000
  , isLoopMemory: true
  , isLoopCell: true
  , cellSize: 256
  }
