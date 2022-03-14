module Brainfk.Web.Store where

import Brainfk.Data.Settings (Settings, defaultSettings)

type Store = { settings :: Record Settings }

initialStore :: Store
initialStore =
  { settings: defaultSettings
  }

data Action = PutSettings (Record Settings)

reduce :: Store -> Action -> Store
reduce store =
  case _ of
    PutSettings settings ->
      store { settings = settings }
