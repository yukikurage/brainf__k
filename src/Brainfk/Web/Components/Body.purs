module Brainfk.Web.Components.Body where

import Prelude

import Brainfk.System.Exec (exec)
import Brainfk.System.Parse (defaultToken, kurageToken, parse)
import Brainfk.Web.Util (css, wrap)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (Component, RefLabel(..), liftAff, liftEffect)
import Halogen.HTML (div_, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onValueChange, onValueInput)
import Halogen.HTML.Properties (readOnly, ref, rows, value)
import Halogen.Hooks (fork, getRef, kill, modify, modify_, put, useState)
import Halogen.Hooks as Hooks
import Web.DOM.Element (scrollHeight, setScrollTop)

component :: forall query input output. Component query input output Aff
component = Hooks.component \_ _ -> Hooks.do
  inputValue /\ inputValueId <- useState ""
  outputText /\ outputTextId <- useState ""
  parseErrorText /\ parseErrorTextId <- useState ""
  runtimeErrorText /\ runtimeErrorTextId <- useState ""
  stopEffect /\ stopEffectId <- useState $ pure unit

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
      case parse defaultToken inputValue of
        Left parseError -> do
          put parseErrorTextId $ show parseError
        Right ast -> do
          { getOutput, waitFinish, getRuntimeError, stop } <- liftEffect
            $ exec { memorySize: 512, chunkNum: 15000 } ""
                ast

          updateForkId <- fork $ forever do
            output <- liftEffect getOutput
            modify_ outputTextId (\prev -> prev <> output)

            autoScroll

            liftAff $ delay $ Milliseconds $ 50.0

          _ <- fork do
            liftAff waitFinish

            runtimeErrorMaybe <- liftEffect getRuntimeError
            kill updateForkId
            output <- liftEffect getOutput
            modify_ outputTextId (\prev -> prev <> output <> "\n")

            autoScroll

            case runtimeErrorMaybe of
              Just runtimeError -> do
                put runtimeErrorTextId $ show runtimeError
              Nothing -> put runtimeErrorTextId $ ""

          put stopEffectId $ do
            liftEffect stop
            kill updateForkId

  Hooks.pure $ HH.div_
    [ HH.div [ css "grid grid-cols-2" ]
        [ textarea
            [ value inputValue
            , onValueInput (put inputValueId)
            , onValueChange (\_ -> runBrainfk)
            , rows 10
            , wrap "off"
            , css ""
            ]
        , textarea
            [ value $ outputText
            , readOnly true
            , rows 6
            , css ""
            , ref $ RefLabel "OutputRef"
            ]
        ]
    , div_ [ text "parse error: ", text parseErrorText ]
    , div_ [ text "runtime error: ", text runtimeErrorText ]
    ]
