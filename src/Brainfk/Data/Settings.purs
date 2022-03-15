module Brainfk.Data.Settings where

import Brainfk.System.Parse as Parse
import Brainfk.System.Transpile as Transpile
import Record (disjointUnion)

type Settings = Transpile.Settings (Parse.Settings ())

defaultSettings :: Record Settings
defaultSettings = disjointUnion Parse.defaultSettings Transpile.defaultSettings
