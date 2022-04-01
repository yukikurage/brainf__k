module Brainfk.Web.Components.HTML.SettingsModal where

import Prelude

import Brainfk.System.Transpile (CellSize(..), Settings)
import Brainfk.Web.Util (css, icon, modifyRecord, putRecord)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.HTML (HTML, button, text)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (InputType(..), checked, name, type_, value)
import Halogen.Hooks (HookM, StateId, put)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent as MouseEvent

settingsModal
  :: forall w m
   . MonadEffect m
  => { isSettingsModalOpen :: Boolean
     , isSettingsModalOpenId :: StateId Boolean
     , settings :: Record (Settings ())
     , settingsId :: StateId (Record (Settings ()))
     }
  -> HTML w (HookM m Unit)
settingsModal
  { isSettingsModalOpen, isSettingsModalOpenId, settings, settingsId } =
  let
    settingsItem label child = HH.div [ css "w-auto flex flex-row" ]
      [ HH.div [ css "w-44" ]
          [ HH.text label
          ]
      , HH.div [ css "w-auto h-auto" ] child
      ]
  in
    HH.div
      [ css $
          "fixed left-0 right-0 top-0 bottom-0 flex items-center justify-center transition-all duration-75 bg-zinc-700 opacity-100 bg-opacity-50 visible "
            <>
              if isSettingsModalOpen then
                ""
              else "invisible opacity-0"
      , onClick \_ -> do
          put isSettingsModalOpenId false
      ] -- Modal
      [ HH.div
          [ css $
              "h-fit w-fit p-6 rounded-sm bg-white transition-all duration-75 "
                <>
                  if isSettingsModalOpen then "" else "scale-[0.96]"
          , onClick \e -> liftEffect $ stopPropagation $ MouseEvent.toEvent e
          ]
          [ HH.div [ css "right-0 flex flex-row mb-6" ]
              [ HH.div [ css "text-2xl" ] [ text "Settings" ]
              , HH.div [ css "flex-grow" ] []
              , button
                  [ onClick \_ -> do
                      put isSettingsModalOpenId false
                  , css
                      "text-zinc-500 transition hover:text-zinc-600 disabled:text-zinc-300"
                  ]
                  [ icon "fa-solid fa-xmark fa-2xl" ]
              ]
          , HH.div [ css "flex flex-col h-full flex-grow gap-3 text-lg" ]
              [ settingsItem "Memory Size"
                  [ HH.input
                      [ css "w-40 font-roboto"
                      , type_ $ InputNumber
                      , value $ show $ settings.memorySize
                      , onValueInput \value -> modifyRecord settingsId
                          \{ memorySize } ->
                            { memorySize: fromMaybe memorySize $
                                fromString value
                            }
                      ]
                  ]
              , settingsItem "Cell Size"
                  [ HH.div
                      [ css "flex flex-col gap-3" ]
                      [ HH.div [ css "flex flex-row gap-2 items-center" ]
                          [ HH.input
                              [ type_ $ InputRadio
                              , checked $ settings.cellSize == Bit8
                              , onValueInput \_ -> putRecord
                                  settingsId
                                  { cellSize: Bit8 }
                              , name "CellSize"
                              ]
                          , HH.text "8 bit"
                          ]
                      , HH.div [ css "flex flex-row gap-2 items-center" ]
                          [ HH.input
                              [ type_ $ InputRadio
                              , checked $ settings.cellSize == Bit16
                              , onValueInput \_ -> putRecord
                                  settingsId
                                  { cellSize: Bit16 }
                              , name "CellSize"
                              ]
                          , HH.text "16 bit"
                          ]
                      , HH.div
                          [ css "flex flex-row gap-2 items-center" ]
                          [ HH.input
                              [ type_ $ InputRadio
                              , checked $ settings.cellSize == Bit32
                              , onValueInput \_ -> putRecord
                                  settingsId
                                  { cellSize: Bit32 }
                              , name "CellSize"
                              ]
                          , HH.text "32 bit"
                          ]
                      ]
                  ]
              , settingsItem "EOI"
                  [ text "No change" ]
              , settingsItem "Cell Overflow"
                  [ text "Mod" ]
              , settingsItem "Memory Overflow"
                  [ text "Undefined" ]
              , settingsItem "New line character"
                  [ text "\\n" ]
              ]
          ]
      ]
