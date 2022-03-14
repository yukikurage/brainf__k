module Brainfk.Data.Settings where

import Prelude

import Brainfk.System.Exec as Exec
import Brainfk.System.Parse as Parse
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

type Settings = Exec.Settings (Parse.Settings ())

settingsToString :: Record Settings -> String
settingsToString = encodeJson >>> stringify

stringToSettings :: String -> Maybe (Record Settings)
stringToSettings str = do
  res <- hush $ parseJson str
  hush $ decodeJson res

saveSettings :: Record Settings -> Effect Unit
saveSettings settings =
  setItem "save_settings" (settingsToString settings) =<< localStorage =<<
    window

loadSettings :: Effect (Maybe (Record Settings))
loadSettings = do
  res <- getItem "save_settings" =<< localStorage =<< window
  pure $ stringToSettings =<< res
