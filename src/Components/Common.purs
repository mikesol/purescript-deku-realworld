module Components.Common where

import Prelude

import Data.Foldable (oneOf)
import Deku.Attribute ((:=))
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Listeners (textInput)
import Effect (Effect)

fieldset
  :: forall s m lock payload
   . Korok s m
  => Boolean
  -> String
  -> (String -> Effect Unit)
  -> Domable m lock payload
fieldset isPw placeholder pusher = D.fieldset (oneOf [ pure $ D.Class := "form-group" ])
  [ D.input
      ( oneOf
          [ pure $ D.Class := "form-control form-control-lg"
          , pure $ D.Xtype := if isPw then "password" else "text"
          , pure $ D.Placeholder := placeholder
          , textInput (pure pusher)
          ]
      )
      []
  ]