module Brainfk.Web.Components.HTML.Header where

import Brainfk.Web.Util (css)
import Halogen.HTML (HTML, text)
import Halogen.HTML as HH
import Halogen.HTML.Properties (href, rel, target)

header :: forall w i. HTML w i
header = HH.div
  [ css "h-12 bg-white px-4 flex flex-row items-center" ] --Header
  [ HH.div
      [ css "flex flex-row  items-end h-auto w-full" ]
      [ HH.div [ css "text-3xl text-zinc-700" ]
          [ text "Yuki Brainf" ]
      , HH.div [ css "text-3xl text-fuchsia-500" ]
          [ text "**" ]
      , HH.div [ css "text-3xl pr-3 text-zinc-700" ]
          [ text "k" ]
      , HH.div [ css "flex-grow" ] []
      , HH.a
          [ css
              "text-xl text-zinc-500 hover:text-zinc-700 cursor-pointer"
          , href "https://yukikurage.github.io/portfolio/#about"
          , target "_blank"
          , rel "noopener"
          ]
          [ text "yukiworks" ]
      ]
  ]
