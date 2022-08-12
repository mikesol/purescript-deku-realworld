module Components.Common where

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
  -> AnEvent m String
  -> String
  -> (String -> Effect Unit)
  -> Domable m lock payload
fieldset isPw value placeholder pusher = D.fieldset (oneOf [ pure $ D.Class := "form-group" ])
  [ D.input
      ( oneOf
          [ pure $ D.Class := "form-control form-control-lg"
          , pure $ D.Xtype := if isPw then "password" else "text"
          , pure $ D.Placeholder := placeholder
          , value <#> (D.Value := _)
          , textInput (pure pusher)
          ]
      )
      []
  ]

textFieldWithValue
  :: forall s m lock payload
   . Korok s m
  => AnEvent m String
  -> String
  -> (String -> Effect Unit)
  -> Domable m lock payload
textFieldWithValue = fieldset false

textField
  :: forall s m lock payload
   . Korok s m
  => String
  -> (String -> Effect Unit)
  -> Domable m lock payload
textField = fieldset false empty

passwordField
  :: forall s m lock payload
   . Korok s m
  => String
  -> (String -> Effect Unit)
  -> Domable m lock payload
passwordField = fieldset true empty
