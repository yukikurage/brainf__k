module Brainfk.System.Transpile
  ( CellSize(..)
  , Settings
  , Transpiled(..)
  , defaultSettings
  , transpile
  ) where

import Prelude


import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)

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

foreign import transpileImpl
  :: forall r
   . { memorySize :: Number, cellSize :: Int | r }
  -> String
  -> Effect Transpiled

transpile
  :: forall r. Record (Settings r) -> String -> Aff Transpiled
transpile settings code = liftEffect $ transpileImpl
  { memorySize: toNumber settings.memorySize
  , cellSize: cellSizeToAlignment settings.cellSize
  }
  code
