module Brainfk.Web.Components.Web.TextareaBase where

import Brainfk.Web.Util (css, wrap)
import Halogen (RefLabel)
import Halogen.HTML (HTML, textarea)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties (readOnly, ref, value)

textareaBase
  :: forall w i
   . { onValueInput :: String -> i
     , ref :: RefLabel
     , value :: String
     , readOnly :: Boolean
     }
  -> HTML w i
textareaBase input = textarea
  [ value input.value
  , onValueInput input.onValueInput
  , wrap "off"
  , css
      "w-full h-full flex-grow resize-none font-roboto border-2 rounded-sm p-1 text-md bg-white text-zinc-700 border-none outline-zinc-300"
  , ref input.ref
  , readOnly input.readOnly
  ]
