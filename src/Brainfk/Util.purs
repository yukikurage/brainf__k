module Brainfk.Util where

import Prelude

import Data.Array (fromFoldable, reverse)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)

whileM :: forall m a. Monad m => m (Maybe a) -> m (Array a)
whileM action = reverse <<< fromFoldable <$> loop
  where
  loop = do
    res <- action
    case res of
      Nothing -> pure Nil
      Just x -> (x : _) <$> loop

foreign import setZeroTimeout :: Effect Unit -> Effect Unit

delayZero :: Aff Unit
delayZero = makeAff \callback -> setZeroTimeout (callback $ Right unit) *> pure
  mempty
