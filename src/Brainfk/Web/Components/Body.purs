module Brainfk.Web.Components.Body where

import Prelude

import Brainfk.System.Exec (exec)
import Brainfk.System.Parse (defaultToken, parse)
import Brainfk.Web.Util (css, icon, wrap)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, Milliseconds(..), delay, message)
import Halogen (Component, RefLabel(..), liftAff, liftEffect)
import Halogen.HTML (button, div_, i, i_, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (readOnly, ref, value)
import Halogen.Hooks (fork, getRef, kill, modify_, put, useState)
import Halogen.Hooks as Hooks
import Web.DOM.Element (scrollHeight, setScrollTop)

component :: forall query input output. Component query input output Aff
component = Hooks.component \_ _ -> Hooks.do
  inputValue /\ inputValueId <- useState ""
  outputText /\ outputTextId <- useState ""
  parseErrorText /\ parseErrorTextId <- useState ""
  stopEffect /\ stopEffectId <- useState $ pure unit
  isRunning /\ isRunningId <- useState false
  stepNum /\ stepNumId <- useState 0

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
      case parse defaultToken inputValue of
        Left parseError -> do
          put parseErrorTextId $ show parseError
        Right ast -> do
          { getOutput, waitFinish, stop, getStep } <- liftEffect
            $ exec { memorySize: 512, chunkNum: 15000 } ""
                ast

          updateForkId <- fork $ forever do
            output <- liftEffect getOutput
            modify_ outputTextId (\prev -> prev <> output)
            step <- liftEffect getStep
            put stepNumId step

            autoScroll

            liftAff $ delay $ Milliseconds $ 50.0

          _ <- fork do
            runtimeErrorMaybe <- liftAff waitFinish

            kill updateForkId

            output <- liftEffect getOutput
            modify_ outputTextId (\prev -> prev <> output)
            step <- liftEffect getStep
            put stepNumId step

            autoScroll

            put isRunningId false

            case runtimeErrorMaybe of
              Just runtimeError -> do
                modify_ outputTextId
                  (\prev -> prev <> "\n" <> message runtimeError)
              Nothing -> pure unit

          put stopEffectId $ do
            liftEffect stop
            kill updateForkId

  Hooks.pure $ HH.div [ css "h-screen w-screen flex flex-col bg-stone-100" ]
    [ HH.div [ css "h-14 pt-2 px-5 flex flex-row items-center" ]
        [ button [ onClick \_ -> runBrainfk, css "py-2 px-4" ]
            [ icon "fa-solid fa-play fa-xl" ]
        , button [ onClick \_ -> stopEffect, css "py-2 px-4" ]
            [ icon "fa-solid fa-pause fa-xl" ]
        ]
    , HH.div
        [ css "grid grid-cols-2 flex-grow" ]
        [ HH.div [ css "h-full flex flex-col p-3" ]
            [ textarea
                [ value inputValue
                , onValueInput \value -> do
                    put inputValueId value
                    case parse defaultToken value of
                      Right _ -> put parseErrorTextId "OK"
                      Left parseError -> do
                        put parseErrorTextId $ show parseError
                , wrap "off"
                , css
                    """w-full
                      flex-grow
                      resize-none
                      font-roboto
                      border-2
                      rounded-md p-2
                      text-lg
                      bg-stone-50
                      border-stone-300
                      outline-stone-500"""
                ]
            , div_ [ text "parse error: ", text parseErrorText ]
            ]
        , HH.div [ css "h-full flex flex-col p-3" ]
            [ textarea
                [ value $ outputText
                , readOnly true
                , css
                    """w-full
                      flex-grow
                      resize-none
                      font-roboto
                      border-2
                      rounded-md p-2
                      text-lg
                      bg-stone-50
                      border-stone-300
                      outline-stone-500"""
                , ref $ RefLabel "OutputRef"
                ]
            , div_ [ text "steps: ", text $ show $ stepNum ]
            , div_ [ text "running: ", text $ show $ isRunning ]
            ]
        ]
    ]
