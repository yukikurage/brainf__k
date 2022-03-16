module Brainfk.System.Exec where

import Prelude

import Brainfk.System.Transpile (Transpiled(..))
import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)

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

-- | execute Transpiled Brainfuck code
exec
  :: Transpiled
  -> Effect
       { getOutput :: Effect String
       , stop :: Effect Unit
       , waitFinish :: Aff (Maybe Error)
       }
exec (Transpiled transpiled) = do
  res@{ waitFinish } <- exec_ Just Nothing
    transpiled
  pure res { waitFinish = toAff waitFinish }
