module Brainfk.System.Transpile
  ( CellSize(..)
  , Settings
  , Transpiled(..)
  , defaultSettings
  , transpile
  ) where

import Prelude

import Data.Int (toNumber)

newtype Transpiled = Transpiled String

derive newtype instance Eq Transpiled
derive newtype instance Ord Transpiled
derive newtype instance Show Transpiled
derive newtype instance Semigroup Transpiled
derive newtype instance Monoid Transpiled

data CellSize = Bit8 | Bit16 | Bit32

derive instance Eq CellSize
derive instance Ord CellSize

cellSizeToString :: CellSize -> String
cellSizeToString Bit8 = "8"
cellSizeToString Bit16 = "16"
cellSizeToString Bit32 = "32"

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

foreign import transpile_
  :: forall r
   . { memorySize :: Number, cellSize :: String | r }
  -> String
  -> String
  -> String

transpile
  :: forall r. Record (Settings r) -> String -> String -> Transpiled
transpile settings code input = Transpiled $ transpile_
  { memorySize: toNumber settings.memorySize
  , cellSize: cellSizeToString settings.cellSize
  }
  code
  input
