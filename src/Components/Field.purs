module Components.Field where

import Prelude

import Control.Plus (empty)
import Data.Foldable (oneOf)
import Deku.Attribute ((:=))
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Listeners (textInput)
import Effect (Effect)
import FRP.Event (Event)

fieldset
  :: forall lock payload
   . Boolean
  -> Boolean
  -> Event String
  -> String
  -> (String -> Effect Unit)
  -> Domable lock payload
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
  :: forall lock payload
   . Event String
  -> String
  -> (String -> Effect Unit)
  -> Domable lock payload
largeTextFieldWithValue = fieldset true false

textFieldWithValue
  :: forall lock payload
   . Event String
  -> String
  -> (String -> Effect Unit)
  -> Domable lock payload
textFieldWithValue = fieldset false false

textField
  :: forall lock payload
   . String
  -> (String -> Effect Unit)
  -> Domable lock payload
textField = fieldset false false empty

largeTextField
  :: forall lock payload
   . String
  -> (String -> Effect Unit)
  -> Domable lock payload
largeTextField = fieldset true false empty

passwordField
  :: forall lock payload
   . String
  -> (String -> Effect Unit)
  -> Domable lock payload
passwordField = fieldset false true empty

largePasswordField
  :: forall lock payload
   . String
  -> (String -> Effect Unit)
  -> Domable lock payload
largePasswordField = fieldset true true empty
