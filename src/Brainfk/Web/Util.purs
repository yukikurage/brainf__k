module Brainfk.Web.Util where

import Prelude

import DOM.HTML.Indexed.WrapValue (WrapValue)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)

wrap :: forall r i. String -> HH.IProp (wrap :: WrapValue | r) i
wrap = HH.prop (HH.PropName "wrap")

css :: forall r i. String -> HH.IProp (class  :: String | r) i
css = class_ <<< ClassName
