module Brainfk.Web.Util where

import DOM.HTML.Indexed.WrapValue (WrapValue)
import Halogen.HTML as HH

wrap :: forall r i. String -> HH.IProp (wrap :: WrapValue | r) i
wrap = HH.prop (HH.PropName "wrap")
