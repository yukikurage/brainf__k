module Brainfk.Web.Components.Body where

import Prelude

import Brainfk.System.Exec (exec)
import Brainfk.System.Transpile (defaultSettings, transpile)
import Brainfk.Web.Components.HTML.ButtonBase (ButtonColor(..), buttonBase)
import Brainfk.Web.Components.HTML.Header (header)
import Brainfk.Web.Components.HTML.SettingsModal (settingsModal)
import Brainfk.Web.Components.HTML.TextareaBase (textareaBase)
import Brainfk.Web.Util (css, diffS, icon)
import Control.Monad.Rec.Class (forever)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (slice)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay, message)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (throw)
import Effect.Now (nowTime)
import Halogen (Component, RefLabel(..), liftAff, liftEffect)
import Halogen.HTML (text)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (disabled, href, readOnly, ref, rel, target, value)
import Halogen.Hooks (fork, getRef, kill, modify_, put, useState)
import Halogen.Hooks as Hooks
import Web.DOM.Element (scrollHeight, setScrollTop)

component
  :: forall query input output m
   . MonadAff m
  => Component query input output m
component = Hooks.component \_ _ -> Hooks.do
  codeValue /\ codeValueId <- useState ""
  inputValue /\ inputValueId <- useState ""
  outputValue /\ outputValueId <- useState ""

  stopEffect /\ stopEffectId <- useState $ pure unit
  isRunning /\ isRunningId <- useState false

  isSettingsModalOpen /\ isSettingsModalOpenId <- useState false
  settings /\ settingsId <- useState defaultSettings

  transpileTime /\ transpileTimeId <- useState 0
  execTime /\ execTimeId <- useState 0

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
      put outputValueId ""
      put isRunningId true
      put transpileTimeId 0
      put execTimeId 0

      transpileBeforeTime <- liftEffect nowTime
      transpiled <- liftAff $ transpile settings codeValue
      transpileAfterTime <- liftEffect nowTime
      put transpileTimeId $ diffS transpileAfterTime transpileBeforeTime

      execBeforeTime <- liftEffect nowTime

      reg <- either (throw >>> liftEffect) pure $ regex "\\r?\\n" global

      let
        input = replace reg "\n" inputValue
      { getOutput, stop, waitFinish } <- liftEffect $ exec input transpiled

      updateForkId <- fork $ forever do
        output <- liftEffect getOutput
        modify_ outputValueId
          ( \prev -> fromMaybe (prev <> output) $ slice (-100000) (-1)
              (prev <> output)
          )

        autoScroll

        liftAff $ delay $ Milliseconds $ 30.0

      _ <- fork do
        runtimeErrorMaybe <- liftAff waitFinish

        kill updateForkId
        output <- liftEffect getOutput
        modify_ outputValueId
          ( \prev -> fromMaybe (prev <> output) $ slice (-100000) (-1)
              (prev <> output)
          )
        autoScroll
        put isRunningId false
        execAfterTime <- liftEffect nowTime

        put execTimeId $ execAfterTime `diffS` execBeforeTime
        case runtimeErrorMaybe of
          Just runtimeError -> do
            modify_ outputValueId
              (\prev -> prev <> "\nError: " <> message runtimeError)
          Nothing -> pure unit

      put stopEffectId $ do
        liftEffect stop
        kill updateForkId

  Hooks.pure $ HH.div
    [ css "h-screen w-screen font-inconsolata text-zinc-700 flex flex-col"
    ]
    [ header
    , HH.div
        [ css
            "flex-grow overflow-auto flex flex-col bg-zinc-100 min-w-[850px] min-h-[550px]"
        ] --main
        [ HH.div [ css "h-11 p-2 flex flex-row items-end" ]
            [ buttonBase ButtonPink
                [ onClick \_ -> runBrainfk
                , disabled (isRunning)
                ]
                [ icon "fa-solid fa-play fa-xl" ]
            , buttonBase ButtonPink
                [ onClick \_ -> stopEffect
                , disabled (not isRunning)
                ]
                [ icon "fa-solid fa-stop fa-xl" ]
            , HH.div [ css "flex-grow" ] []
            , buttonBase ButtonBlack []
                [ HH.a
                    [ href "https://github.com/yukikurage/brainf__k"
                    , target "_blank"
                    , rel "noopener"
                    ]
                    [ icon "fa-brands fa-github fa-xl" ]
                ]
            , buttonBase ButtonBlack
                [ onClick \_ -> put isSettingsModalOpenId true
                ]
                [ icon "fa-solid fa-gear fa-xl" ]
            ]
        , HH.div
            [ css "flex flex-row flex-grow" ]
            [ HH.div [ css "h-full flex-[8] flex flex-col p-1" ]
                [ textareaBase
                    [ value codeValue
                    , onValueInput $ put codeValueId
                    ]
                ]
            , HH.div [ css "h-full flex-[7] flex flex-col" ]
                [ HH.div [ css "text-xl p-1" ] [ text "Input" ]
                , HH.div [ css "w-full flex-[2] p-1" ]
                    [ textareaBase
                        [ value inputValue
                        , onValueInput $ put inputValueId
                        ]
                    ]
                , HH.div [ css "text-xl p-1" ] [ text "Output" ]
                , HH.div [ css "w-full flex-[7] p-1" ]
                    [ textareaBase
                        [ value outputValue
                        , ref $ RefLabel "OutputRef"
                        , readOnly true
                        ]
                    ]
                ]
            ]
        , HH.div [ css "p-1" ]
            [ text $
                if isRunning then "Running"
                else "Total time: "
                  <> show (transpileTime + execTime)
                  <> " (Transpile: "
                  <> show transpileTime
                  <> ", Execute: "
                  <> show execTime
                  <> ") ms"
            ]
        ]
    , settingsModal
        { isSettingsModalOpen, isSettingsModalOpenId, settings, settingsId }
    ]
