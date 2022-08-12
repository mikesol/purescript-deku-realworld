module Components.Login where

import Prelude

import API.Effects (logIn)
import API.Types (User)
import Control.Alt ((<|>))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (blank, switcher, text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Deku
import Deku.Listeners (click, textInput)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Foreign.Object (toUnfoldable)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

login_ =
  Proxy    :: Proxy
         """<div class="auth-page">
    <div class="container page">
        <div class="row">

            <div class="col-md-6 offset-md-3 col-xs-12">
                <h1 class="text-xs-center">Sign in</h1>
                ~formMatter~
            </div>

        </div>
    </div>
</div>"""

login :: forall s m lock payload. Korok s m => (User -> Effect Unit) -> Domable m lock payload
login setCurrentUser = login_ ~~
  { formMatter: nut
      ( Deku.do
          let
            fieldset :: String -> (String -> Effect Unit) -> Domable m lock payload
            fieldset placeholder pusher = D.fieldset (oneOf [ pure $ D.Class := "form-group" ])
              [ D.input
                  ( oneOf
                      [ pure $ D.Class := "form-control form-control-lg"
                      , pure $ D.Xtype := "text"
                      , pure $ D.Placeholder := placeholder
                      , textInput (pure pusher)
                      ]
                  )
                  []
              ]
          setErrors /\ errors <- useState []
          setEmail /\ email <- useState Nothing
          setPassword /\ password <- useState Nothing
          let errorMessages = ((email <|> password <|> pure Nothing) $> []) <|> errors
          D.div_
            [ D.div_
                [ errorMessages # switcher case _ of
                    [] -> blank
                    errs -> D.ul (oneOf [ pure $ D.Class := "error-messages" ])
                      (map (D.li_ <<< pure <<< text_) errs)
                ]
            , D.div_
                [ fieldset "Email" (Just >>> setEmail)
                , fieldset "Password" (Just >>> setPassword)
                , D.button
                    ( oneOf
                        [ pure $ D.Class := "btn btn-lg btn-primary pull-xs-right"
                        , click $
                            ( { email: _, password: _ }
                                <$> email
                                <*> password
                            ) <#> \fields -> do
                              let
                                parsed = { email: _, password: _ }
                                  <$> withErrors [ "Email cannot be empty" ] fields.email
                                  <*> withErrors [ "Password cannot be empty" ] fields.password
                              case runExcept parsed of
                                Left errs -> setErrors errs
                                Right user -> launchAff_ do
                                  resp <- logIn { user }
                                  liftEffect case resp of
                                    Left { errors: errs } -> setErrors (map (\(a /\ b) -> a <> " " <> intercalate ", " b) (toUnfoldable errs))
                                    Right currentUser -> do
                                      setCurrentUser currentUser.user
                                      window >>= location >>= setHref "/#/"
                        ]
                    )
                    [ text_ "Sign in" ]
                ]
            ]
      )
  }
  where
  withErrors :: Array String -> Maybe String -> Except (Array String) String
  withErrors e = maybe (throwError e) pure