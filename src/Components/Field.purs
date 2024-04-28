module Components.Field where

import Prelude

import Control.Plus (empty)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners (valueOn_)
import Deku.DOM.Listeners as DL
import Effect (Effect)
import FRP.Poll (Poll)

fieldset
  :: Boolean
  -> Boolean
  -> Poll String
  -> String
  -> (String -> Effect Unit)
  -> Nut
fieldset isLg isPw value placeholder pusher = D.fieldset [ DA.klass_ "form-group" ]
  [ D.input
      [ DA.klass_ $ "form-control" <> (if isLg then " form-control-lg" else "")
      , DA.xtype_ if isPw then "password" else "text"
      , DA.placeholder_ placeholder
      , DA.value value
      , valueOn_ DL.input pusher
      ]
      []
  ]

largeTextFieldWithValue
  :: Poll String
  -> String
  -> (String -> Effect Unit)
  -> Nut
largeTextFieldWithValue = fieldset true false

textFieldWithValue
  :: Poll String
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
