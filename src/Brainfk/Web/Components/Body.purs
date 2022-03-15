module Brainfk.Web.Components.Body where

import Prelude

import Brainfk.Data.Settings (defaultSettings)
import Brainfk.System.Parse (parse)
import Brainfk.System.Transpile (exec)
import Brainfk.Web.Util (css, icon, modifyRecord, putRecord, wrap)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (slice)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay, message)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, RefLabel(..), liftAff, liftEffect)
import Halogen.HTML (button, div_, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (InputType(..), disabled, href, readOnly, ref, rel, target, type_, value)
import Halogen.Hooks (fork, getRef, kill, modify_, put, useState)
import Halogen.Hooks as Hooks
import Web.DOM.Element (scrollHeight, setScrollTop)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent as MouseEvent

component
  :: forall query input output m
   . MonadAff m
  => Component query input output m
component = Hooks.component \_ _ -> Hooks.do
  codeValue /\ codeValueId <- useState ""
  inputValue /\ inputValueId <- useState ""
  outputText /\ outputTextId <- useState ""
  parseErrorText /\ parseErrorTextId <- useState ""
  stopEffect /\ stopEffectId <- useState $ pure unit
  isRunning /\ isRunningId <- useState false
  isParseError /\ isParseErrorId <- useState false
  isSettingsModalOpen /\ isSettingsModalOpenId <- useState false
  settings /\ settingsId <- useState defaultSettings

  let
    checkParseError v = case parse settings v of
      Right _ -> do
        put parseErrorTextId "OK"
        put isParseErrorId false
      Left parseError -> do
        put parseErrorTextId $ show parseError
        put isParseErrorId true

    autoScroll = do
      outputRef <- getRef $ RefLabel "OutputRef"

      liftEffect case outputRef of
        Just r -> do
          h <- scrollHeight r
          setScrollTop h r
        _ -> pure unit

    runBrainfk = do
      stopEffect
      put outputTextId ""
      put parseErrorTextId ""
      put isRunningId true
      case parse settings codeValue of
        Left parseError -> do
          put parseErrorTextId $ show parseError
        Right ast -> do
          { getOutput, stop, waitFinish } <- liftEffect
            $ exec settings ast inputValue

          updateForkId <- fork $ forever do
            output <- liftEffect getOutput
            modify_ outputTextId
              ( \prev -> fromMaybe (prev <> output) $ slice (-100000) (-1)
                  (prev <> output)
              )

            autoScroll

            liftAff $ delay $ Milliseconds $ 100.0

          _ <- fork do
            runtimeErrorMaybe <- liftAff waitFinish
            kill updateForkId
            output <- liftEffect getOutput
            modify_ outputTextId
              ( \prev -> fromMaybe (prev <> output) $ slice (-100000) (-1)
                  (prev <> output)
              )
            autoScroll
            put isRunningId false
            case runtimeErrorMaybe of
              Just runtimeError -> do
                modify_ outputTextId
                  (\prev -> prev <> "\nError: " <> message runtimeError)
              Nothing -> pure unit

          put stopEffectId $ do
            liftEffect stop
            kill updateForkId

  let
    settingsItem label child = HH.div [ css "w-auto flex flex-row" ]
      [ HH.div [ css "w-44" ]
          [ HH.text label
          ]
      , HH.div [ css "w-auto h-auto" ] child
      ]

  Hooks.pure $ HH.div
    [ css
        "h-screen w-screen font-inconsolata text-zinc-600 flex flex-col"
    ]
    [ HH.div [ css "h-12 bg-white px-5 flex flex-row items-center" ] --Header
        [ HH.div
            [ css "flex flex-row  items-end h-auto" ]
            [ HH.div [ css "text-4xl pl-3 text-zinc-600" ]
                [ text "Brainf" ]
            , HH.div [ css "text-4xl text-fuchsia-600" ]
                [ text "**" ]
            , HH.div [ css "text-4xl pr-3 text-zinc-600" ]
                [ text "k" ]
            , HH.div [ css "text-xl pr-3 text-zinc-500" ]
                [ text "interpreter by" ]
            , HH.a
                [ css
                    "text-xl text-zinc-500 hover:text-zinc-600 cursor-pointer"
                , href "https://twitter.com/yukikurage_2019"
                , target "_blank"
                , rel "noopener"
                ]
                [ text "yukikurage" ]
            ]
        ]
    , HH.div
        [ css "flex-grow overflow-auto flex flex-col bg-zinc-100" ] --main
        [ HH.div [ css "h-11 p-2 flex flex-row items-end" ]
            [ button
                [ onClick \_ -> runBrainfk
                , css
                    "px-4 text-fuchsia-500 transition hover:text-fuchsia-600 disabled:text-fuchsia-300"
                , disabled (isRunning || isParseError)
                ]
                [ icon "fa-solid fa-play fa-xl" ]
            , button
                [ onClick \_ -> stopEffect
                , css
                    "px-4 text-fuchsia-500 transition hover:text-fuchsia-600 disabled:text-fuchsia-300"
                , disabled (not isRunning)
                ]
                [ icon "fa-solid fa-stop fa-xl" ]
            , HH.div [ css "flex-grow" ] []
            , button
                [ onClick \_ -> put isSettingsModalOpenId true
                , css
                    "px-4 text-zinc-500 transition hover:text-zinc-600 disabled:text-zinc-300"
                ]
                [ icon "fa-solid fa-gear fa-xl" ]
            ]
        , HH.div
            [ css "flex flex-row flex-grow" ]
            [ HH.div [ css "h-full flex-[6] flex flex-col p-1" ]
                [ textarea
                    [ value codeValue
                    , onValueInput \value -> do
                        put codeValueId value
                        case parse settings value of
                          Right _ -> do
                            put parseErrorTextId "OK"
                            put isParseErrorId false
                          Left parseError -> do
                            put parseErrorTextId $ show parseError
                            put isParseErrorId true
                    , wrap "off"
                    , css
                        """w-full
                      flex-grow
                      resize-none
                      font-roboto
                      border-2
                      rounded-sm
                      p-1
                      text-md
                      bg-white
                      text-zinc-700
                      border-none
                      outline-zinc-300"""
                    ]
                , div_ [ text "parse error: ", text parseErrorText ]
                ]
            , HH.div [ css "h-full flex-[5] flex flex-col" ]
                [ HH.div [ css "text-xl p-1" ] [ text "Input" ]
                , HH.div [ css "w-full flex-[2] p-1" ]
                    [ textarea
                        [ value inputValue
                        , onValueInput \value -> put inputValueId value
                        , wrap "off"
                        , css
                            """
                      resize-none
                      w-full
                      h-full
                      font-roboto
                      border-2
                      rounded-sm
                      p-1
                      text-md
                      bg-white
                      text-zinc-700
                      border-none
                      outline-zinc-300"""
                        , ref $ RefLabel "OutputRef"
                        ]
                    ]
                , HH.div [ css "text-xl p-1" ] [ text "Output" ]
                , HH.div [ css "w-full flex-[7] p-1" ]
                    [ textarea
                        [ value $ outputText
                        , readOnly true
                        , wrap "off"
                        , css
                            """
                      w-full
                      h-full
                      flex-grow
                      resize-none
                      font-roboto
                      border-2
                      rounded-sm
                      p-2
                      text-md
                      bg-white
                      text-zinc-700
                      border-none
                      outline-none"""
                        , ref $ RefLabel "OutputRef"
                        ]
                    ]
                ]
            ]
        ]
    , HH.div
        [ css $
            "fixed left-0 right-0 top-0 bottom-0 flex items-center justify-center transition-all duration-75 bg-zinc-700 opacity-100 bg-opacity-50 visible "
              <>
                if isSettingsModalOpen then
                  ""
                else "invisible opacity-0"
        , onClick \_ -> do
            put isSettingsModalOpenId false
            checkParseError codeValue
        ] -- Modal
        [ HH.div
            [ css $
                "h-fit w-fit p-6 rounded-sm bg-white transition-all duration-75 "
                  <>
                    if isSettingsModalOpen then "" else "scale-[0.96]"
            , onClick \e -> liftEffect $ stopPropagation $ MouseEvent.toEvent e
            ]
            [ HH.div [ css "right-0 flex flex-row justify-end mb-3" ]
                [ button
                    [ onClick \_ -> do
                        put isSettingsModalOpenId false
                        checkParseError codeValue
                    , css
                        "text-zinc-500 transition hover:text-zinc-600 disabled:text-zinc-300"
                    ]
                    [ icon "fa-solid fa-xmark fa-2xl" ]
                ]
            , HH.div [ css "flex flex-row w-full gap-6" ]
                [ HH.div [ css "flex flex-col h-full flex-grow gap-3 text-lg" ]
                    [ HH.div [ css "text-xl" ] [ text "Parse Settings" ]
                    , settingsItem "Pointer Increment"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.pointerIncrement
                            , onValueInput \value -> putRecord settingsId
                                { pointerIncrement: value }
                            ]
                        ]
                    , settingsItem "Pointer Decrement"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.pointerDecrement
                            , onValueInput \value -> putRecord settingsId
                                { pointerDecrement: value }
                            ]
                        ]
                    , settingsItem "Value Increment"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.referenceIncrement
                            , onValueInput \value -> putRecord settingsId
                                { referenceIncrement: value }
                            ]
                        ]
                    , settingsItem "Value Decrement"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.referenceDecrement
                            , onValueInput \value -> putRecord settingsId
                                { referenceDecrement: value }
                            ]
                        ]
                    , settingsItem "Output"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.output
                            , onValueInput \value -> putRecord settingsId
                                { output: value }
                            ]
                        ]
                    , settingsItem "Input"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.input
                            , onValueInput \value -> putRecord settingsId
                                { input: value }
                            ]
                        ]
                    , settingsItem "Loop Start"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.loopStart
                            , onValueInput \value -> putRecord settingsId
                                { loopStart: value }
                            ]
                        ]
                    , settingsItem "Loop End"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , value $ settings.loopEnd
                            , onValueInput \value -> putRecord settingsId
                                { loopEnd: value }
                            ]
                        ]
                    ]
                , HH.div [ css "flex flex-col h-full flex-grow gap-3 text-lg" ]
                    [ HH.div [ css "text-xl" ] [ text "Exec Settings" ]
                    , settingsItem "Memory Size"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , type_ $ InputNumber
                            , value $ show $ settings.memorySize
                            , onValueInput \value -> modifyRecord settingsId
                                \{ memorySize } ->
                                  { memorySize: fromMaybe memorySize $
                                      Int.fromString value
                                  }
                            ]
                        ]
                    , settingsItem "Cell Size"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , type_ $ InputNumber
                            , value $ show $ settings.cellSize
                            , onValueInput \value -> modifyRecord settingsId
                                \{ cellSize } ->
                                  { cellSize: fromMaybe cellSize $
                                      Int.fromString value
                                  }

                            ]
                        ]
                    , settingsItem "Output Threshold"
                        [ HH.input
                            [ css "w-40 font-roboto"
                            , type_ $ InputNumber
                            , value $ show $ settings.chunkNum
                            , onValueInput \value -> modifyRecord settingsId
                                \{ chunkNum } ->
                                  { chunkNum: fromMaybe chunkNum $
                                      Int.fromString value
                                  }

                            ]
                        ]
                    ]
                ]

            ]

        ]
    ]
