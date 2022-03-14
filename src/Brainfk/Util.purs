module Brainfk.Util where

import Prelude

import Data.Array (fromFoldable, reverse)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Prim.Row (class Cons, class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))

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

class
  MapRecord (t :: Type -> Type) (r1 :: Row Type) (r2 :: Row Type)
  | t r1 -> r2 where
  mapRecord
    :: (forall l a. IsSymbol l => Proxy l -> a -> t a)
    -> Record r1
    -> Record r2

instance (IsSymbol l, Cons l a () r1, Cons l (t a) () r2) => MapRecord t r1 r2 where
  mapRecord f r = Record.modify (Proxy :: Proxy l) (f (Proxy :: Proxy l)) r

else instance
  ( IsSymbol l
  , Cons l a r1 r1'
  , Cons l (t a) r2 r2'
  , Lacks l r1
  , Lacks l r2
  , MapRecord t r1 r2
  ) =>
  MapRecord t r1' r2' where
  mapRecord f r =
    let
      prev = Record.delete (Proxy :: Proxy l) r
      now = Record.get (Proxy :: Proxy l) r
    in
      Record.insert (Proxy :: Proxy l) (f (Proxy :: Proxy l) now) $ mapRecord
        f
        prev

class
  UniMapRecord (b :: Type) (r1 :: Row Type) (r2 :: Row Type)
  | b r1 -> r2 where
  uniMapRecord
    :: (forall l a. IsSymbol l => Proxy l -> a -> b)
    -> Record r1
    -> Record r2

instance (IsSymbol l, Cons l a () r1, Cons l b () r2) => UniMapRecord b r1 r2 where
  uniMapRecord f r = Record.modify (Proxy :: Proxy l) (f (Proxy :: Proxy l)) r

else instance
  ( IsSymbol l
  , Cons l a r1 r1'
  , Cons l b r2 r2'
  , Lacks l r1
  , Lacks l r2
  , UniMapRecord b r1 r2
  ) =>
  UniMapRecord b r1' r2' where
  uniMapRecord f r =
    let
      prev = Record.delete (Proxy :: Proxy l) r
      now = Record.get (Proxy :: Proxy l) r
    in
      Record.insert (Proxy :: Proxy l) (f (Proxy :: Proxy l) now) $ uniMapRecord
        f
        prev
