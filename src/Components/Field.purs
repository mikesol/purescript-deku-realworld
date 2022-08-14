module Components.Field where

import Prelude

import Control.Plus (empty)
import Data.Foldable (oneOf)
import Deku.Attribute ((:=))
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Listeners (textInput)
import Effect (Effect)
import FRP.Event (AnEvent)

fieldset
  :: forall s m lock payload
   . Korok s m
  => Boolean
  -> Boolean
  -> AnEvent m String
  -> String
  -> (String -> Effect Unit)
  -> Domable m lock payload
fieldset isLg isPw value placeholder pusher = D.fieldset (oneOf [ pure $ D.Class := "form-group" ])
  [ D.input
      ( oneOf
          [ pure $ D.Class := "form-control" <> (if isLg then " form-control-lg" else "")
          , pure $ D.Xtype := if isPw then "password" else "text"
          , pure $ D.Placeholder := placeholder
          , value <#> (D.Value := _)
          , textInput (pure pusher)
          ]
      )
      []
  ]

largeTextFieldWithValue
  :: forall s m lock payload
   . Korok s m
  => AnEvent m String
  -> String
  -> (String -> Effect Unit)
  -> Domable m lock payload
largeTextFieldWithValue = fieldset true false

textFieldWithValue
  :: forall s m lock payload
   . Korok s m
  => AnEvent m String
  -> String
  -> (String -> Effect Unit)
  -> Domable m lock payload
textFieldWithValue = fieldset false false

textField
  :: forall s m lock payload
   . Korok s m
  => String
  -> (String -> Effect Unit)
  -> Domable m lock payload
textField = fieldset false false empty

largeTextField
  :: forall s m lock payload
   . Korok s m
  => String
  -> (String -> Effect Unit)
  -> Domable m lock payload
largeTextField = fieldset true false empty

passwordField
  :: forall s m lock payload
   . Korok s m
  => String
  -> (String -> Effect Unit)
  -> Domable m lock payload
passwordField = fieldset false true empty

largePasswordField
  :: forall s m lock payload
   . Korok s m
  => String
  -> (String -> Effect Unit)
  -> Domable m lock payload
largePasswordField = fieldset true true empty
