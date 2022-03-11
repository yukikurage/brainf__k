module Brainfk.Web.Components.Body where

import Prelude

import Brainfk.System.Exec (exec)
import Brainfk.System.Parse (parse)
import Brainfk.Web.Util (wrap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, liftAff, liftEffect)
import Halogen.HTML (div_, text, textarea)
import Halogen.HTML.Events (onValueChange, onValueInput)
import Halogen.HTML.Properties (rows, value)
import Halogen.Hooks (fork, kill, put, useState)
import Halogen.Hooks as Hooks

component :: forall query input output m. MonadAff m => Component query input output m
component = Hooks.component \_ _ -> Hooks.do
  inputValue /\ inputValueId <- useState ""
  outputText /\ outputTextId <- useState ""
  parseErrorText /\ parseErrorTextId <- useState ""
  runtimeErrorText /\ runtimeErrorTextId <- useState ""
  execForkId /\ execForkIdId <- useState Nothing

  let
    runBrainfk = do
      case execForkId of
        Just forkId -> kill forkId
        Nothing -> pure unit
      forkId <- fork case parse inputValue of
        Left parseError -> do
          put parseErrorTextId $ show parseError
        Right ast -> do
          { output, result } <- liftEffect $ exec { memorySize: 1000 } "" ast
          runtimeErrorMaybe <- liftAff result
          case runtimeErrorMaybe of
            Just runtimeError -> do
              put runtimeErrorTextId $ show runtimeError
            Nothing -> do
              output' <- liftEffect output
              put outputTextId output'
      put execForkIdId $ Just forkId

  Hooks.pure $ div_
    [ textarea [ value inputValue, onValueInput (put inputValueId), onValueChange (\_ -> runBrainfk), rows 10, wrap "off" ]
    , div_ [ text "output: ", text outputText ]
    , div_ [ text "parse error: ", text parseErrorText ]
    , div_ [ text "runtime error: ", text runtimeErrorText ]
    ]
