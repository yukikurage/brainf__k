module Brainfk.Web.Components.Web.TextareaBase where

import Prelude

import Brainfk.Web.Util (css, wrap)
import DOM.HTML.Indexed (HTMLtextarea)
import Halogen.HTML (HTML, IProp, textarea)

textareaBase
  :: forall w i
   . Array (IProp HTMLtextarea i)
  -> HTML w i
textareaBase props =
  textarea $
    [ wrap "off"
    , css
        "w-full h-full flex-grow resize-none font-roboto border-2 rounded-sm p-1 text-md bg-white text-zinc-700 border-none outline-zinc-300"
    ] <> props
