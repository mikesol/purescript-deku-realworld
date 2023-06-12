module Components.Field where

import Prelude

import Control.Plus (empty)
import Deku.Attribute ((!:=), (<:=>))
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Listeners (textInput)
import Effect (Effect)
import FRP.Event (Event)

fieldset
  :: Boolean
  -> Boolean
  -> Event String
  -> String
  -> (String -> Effect Unit)
  -> Nut
fieldset isLg isPw value placeholder pusher = D.fieldset [ D.Class !:= "form-group" ]
  [ D.input
      [ D.Class !:= "form-control" <> (if isLg then " form-control-lg" else "")
      , D.Xtype !:= if isPw then "password" else "text"
      , D.Placeholder !:= placeholder
      , D.Value <:=> value
      , textInput (pure pusher)
      ]
      []
  ]

largeTextFieldWithValue
  :: Event String
  -> String
  -> (String -> Effect Unit)
  -> Nut
largeTextFieldWithValue = fieldset true false

textFieldWithValue
  :: Event String
  -> String
  -> (String -> Effect Unit)
  -> Nut
textFieldWithValue = fieldset false false

textField
  :: String
  -> (String -> Effect Unit)
  -> Nut
textField = fieldset false false empty

largeTextField
  :: String
  -> (String -> Effect Unit)
  -> Nut
largeTextField = fieldset true false empty

passwordField
  :: String
  -> (String -> Effect Unit)
  -> Nut
passwordField = fieldset false true empty

largePasswordField
  :: String
  -> (String -> Effect Unit)
  -> Nut
largePasswordField = fieldset true true empty
