module Brainfk.Web.Util where

import Prelude

import DOM.HTML.Indexed.WrapValue (WrapValue)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks
import Prim.Row (class Nub, class Union)
import Record as Record

wrap :: forall r i. String -> HH.IProp (wrap :: WrapValue | r) i
wrap = HH.prop (HH.PropName "wrap")

css :: forall r i. String -> HH.IProp (class :: String | r) i
css = class_ <<< ClassName

icon :: forall w i. String -> HTML w i
icon classes = HH.i [ css classes ] []

putRecord
  :: forall m r1 r2 r3
   . Union r1 r2 r3
  => Nub r3 r2
  => StateId (Record r2)
  -> Record r1
  -> HookM m Unit
putRecord stateId new = Hooks.modify_ stateId \prev -> Record.merge new prev

modifyRecord
  :: forall m r1 r2 r3
   . Union r1 r2 r3
  => Nub r3 r2
  => StateId (Record r2)
  -> (Record r2 -> Record r1)
  -> HookM m Unit
modifyRecord stateId f = Hooks.modify_ stateId \prev -> Record.merge (f prev)
  prev
