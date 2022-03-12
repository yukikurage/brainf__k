module Brainfk.Web.Components.ColoredText where

import Prelude

import Brainfk.Web.Util (css)
import Halogen.HTML (HTML(..))

type Segment =
  { tailwindColor :: String
  , text :: String
  }
