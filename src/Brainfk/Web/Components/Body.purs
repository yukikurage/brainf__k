module Brainfk.Web.Components.Body where

import Prelude

import Brainfk.System.Exec (exec)
import Brainfk.System.Parse (defaultSettings, parse)
import Brainfk.Web.Util (css, icon, wrap)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (slice)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, Milliseconds(..), delay, message)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, RefLabel(..), liftAff, liftEffect)
import Halogen.HTML (button, div_, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (disabled, href, readOnly, ref, rel, target, value)
import Halogen.Hooks (fork, getRef, kill, modify_, put, useState)
import Halogen.Hooks as Hooks
import Web.DOM.Element (scrollHeight, setScrollTop)
import Web.Event.Event (preventDefault, stopPropagation)
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
  stepNum /\ stepNumId <- useState 0
  isParseError /\ isParseErrorId <- useState false
  isSettingsModalOpen /\ isSettingsModalOpenId <- useState false

  let
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
      put stepNumId 0
      put isRunningId true
      case parse defaultSettings codeValue of
        Left parseError -> do
          put parseErrorTextId $ show parseError
        Right ast -> do
          { getOutput, waitFinish, stop, getStep } <- liftEffect
            $ exec
                { memorySize: 256
                , chunkNum: 15000
                , isLoopMemory: true
                , isLoopCell: true
                , cellSize: 256
                }
                inputValue
                ast

          updateForkId <- fork $ forever do
            output <- liftEffect getOutput
            modify_ outputTextId
              ( \prev -> fromMaybe (prev <> output) $ slice (-100000) (-1)
                  (prev <> output)
              )
            step <- liftEffect getStep
            put stepNumId step

            autoScroll

            liftAff $ delay $ Milliseconds $ 50.0

          _ <- fork do
            runtimeErrorMaybe <- liftAff waitFinish

            kill updateForkId

            output <- liftEffect getOutput
            modify_ outputTextId
              ( \prev -> fromMaybe (prev <> output) $ slice (-100000) (-1)
                  (prev <> output)
              )
            step <- liftEffect getStep
            put stepNumId step

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
                        case parse defaultSettings value of
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
                        , wrap "soft"
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
                , div_ [ text "steps: ", text $ show $ stepNum ]
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
        , onClick \_ -> put isSettingsModalOpenId false
        ] -- Modal
        [ HH.div
            [ css $
                "h-fit w-fit p-4 rounded-sm bg-white transition-all duration-75 "
                  <>
                    if isSettingsModalOpen then "" else "scale-[0.96]"
            , onClick \e -> liftEffect $ stopPropagation $ MouseEvent.toEvent e
            ]
            [ HH.div [ css "right-0 flex flex-row justify-end mb-3" ]
                [ button
                    [ onClick \_ -> put isSettingsModalOpenId false
                    , css
                        "text-zinc-500 transition hover:text-zinc-600 disabled:text-zinc-300"
                    ]
                    [ icon "fa-solid fa-xmark fa-2xl" ]
                ]
            , HH.div [ css "flex flex-row w-full gap-4" ]
                [ HH.div [ css "flex flex-col h-full flex-grow gap-2" ]
                    [ HH.div [ css "text-lg" ] [ text "Parse Settings" ] ]
                , HH.div [ css "flex flex-col h-full flex-grow gap-2" ]
                    [ HH.div [ css "text-lg" ] [ text "Exec Settings" ] ]
                ]

            ]

        ]
    ]
