module Main where

import Prelude

import Brainfk.Web.Components.Body as Body
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Halogen.Aff (awaitLoad, selectElement)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = launchAff_ do
  log "Starting Halogen app"
  awaitLoad
  body <- selectElement $ QuerySelector "body"
  traverse_ (runUI Body.component unit) body
