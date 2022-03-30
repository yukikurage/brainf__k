module Brainfk.System.Transpile
  ( CellSize(..)
  , Settings
  , Transpiled(..)
  , defaultSettings
  , transpile
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Int (toNumber)
import Effect.Aff (Aff)

foreign import data Transpiled :: Type

data CellSize = Bit8 | Bit16 | Bit32

derive instance Eq CellSize
derive instance Ord CellSize

cellSizeToAlignment :: CellSize -> Int
cellSizeToAlignment Bit8 = 0
cellSizeToAlignment Bit16 = 1
cellSizeToAlignment Bit32 = 2

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
   . { memorySize :: Number, cellSize :: Int | r }
  -> String
  -> Promise Transpiled

transpile
  :: forall r. Record (Settings r) -> String -> Aff Transpiled
transpile settings code = toAff $ transpile_
  { memorySize: toNumber settings.memorySize
  , cellSize: cellSizeToAlignment settings.cellSize
  }
  code
