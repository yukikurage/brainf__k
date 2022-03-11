module Brainfk.Util where

import Prelude

import Data.Array (fromFoldable, reverse)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

whileM :: forall m a. Monad m => m (Maybe a) -> m (Array a)
whileM action = reverse <<< fromFoldable <$> loop
  where
  loop = do
    res <- action
    case res of
      Nothing -> pure Nil
      Just x -> (x : _) <$> loop
