module Brainfk.Web.Store where

import Brainfk.Data.Settings (Settings)
import Brainfk.System.Exec as Exec
import Brainfk.System.Parse as Parse
import Record (disjointUnion)

type Store = { settings :: Record Settings }

initialStore :: Store
initialStore =
  { settings: disjointUnion Parse.defaultSettings Exec.defaultSettings
  }

data Action = PutSettings (Record Settings)

reduce :: Store -> Action -> Store
reduce store =
  case _ of
    PutSettings settings ->
      store { settings = settings }
