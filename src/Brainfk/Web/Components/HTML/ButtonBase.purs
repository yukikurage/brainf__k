module Brainfk.Web.Components.HTML.ButtonBase where

import Prelude

import Brainfk.Web.Util (css)
import DOM.HTML.Indexed (HTMLbutton)
import Halogen.HTML (HTML, IProp, button)

data ButtonColor = ButtonPink | ButtonBlack

derive instance Eq ButtonColor
derive instance Ord ButtonColor

buttonColorToCSS :: ButtonColor -> String
buttonColorToCSS = case _ of
  ButtonPink ->
    "px-4 text-fuchsia-500 transition hover:text-fuchsia-700 disabled:text-fuchsia-300"
  ButtonBlack ->
    "px-4 text-zinc-500 transition hover:text-zinc-600 disabled:text-zinc-300"

buttonBase
  :: forall t8 t9
   . ButtonColor
  -> Array (IProp HTMLbutton t9)
  -> Array (HTML t8 t9)
  -> HTML t8 t9
buttonBase buttonColor props = button $ [ css $ buttonColorToCSS buttonColor ]
  <> props
