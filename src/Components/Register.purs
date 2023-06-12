module Components.Register where

import Prelude

import API.Effects as Effects
import API.Types (User)
import Components.Field (largePasswordField, largeTextField)
import Control.Alt ((<|>))
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, toEither)
import Deku.Attribute ((!:=))
import Deku.Attributes (klass_)
import Deku.Control (blank, text_, (<#~>))
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Listeners (click)
import Deku.Pursx ((~~))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Foreign.Object (toUnfoldable)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

register_ =
  Proxy
    :: Proxy
         """<div class="auth-page">
    <div class="container page">
        <div class="row">

            <div class="col-md-6 offset-md-3 col-xs-12">
                <h1 class="text-xs-center">Sign up</h1>
                <p class="text-xs-center">
                    <a href="/#/login">Have an account?</a>
                </p>

                ~formMatter~
            </div>

        </div>
    </div>
</div>"""

register :: (User -> Effect Unit) -> Nut
register setCurrentUser = register_ ~~
  { formMatter: fixed
      [ Deku.do
          setErrors /\ errors <- useState []
          setName /\ name <- useState Nothing
          setEmail /\ email <- useState Nothing
          setPassword /\ password <- useState Nothing
          let errorMessages = ((email <|> password <|> pure Nothing) $> []) <|> errors
          D.div_
            [ D.div_
                [ errorMessages <#~> case _ of
                    [] -> blank
                    errs -> D.ul [ D.Class !:= "error-messages" ]
                      (map (D.li_ <<< pure <<< text_) errs)
                ]
            , D.div_
                [ largeTextField "Your Name" (Just >>> setName)
                , largeTextField "Email" (Just >>> setEmail)
                , largePasswordField "Password" (Just >>> setPassword)
                , D.button
                    [ klass_  "btn btn-lg btn-primary pull-xs-right"
                    , click $
                        ( { email: _, password: _, username: _ }
                            <$> email
                            <*> password
                            <*> name
                        ) <#> \fields -> do
                          let
                            parsed = { email: _, password: _, username: _ }
                              <$> withErrors [ "Email cannot be empty" ] fields.email
                              <*> withErrors [ "Password cannot be empty" ] fields.password
                              <*> withErrors [ "Username cannot be empty" ] fields.username
                          case toEither parsed of
                            Left errs -> setErrors errs
                            Right user -> launchAff_ do
                              resp <- Effects.register { user }
                              liftEffect case resp of
                                Left { errors: errs } -> setErrors (map (\(a /\ b) -> a <> " " <> intercalate ", " b) (toUnfoldable errs))
                                Right currentUser -> do
                                  setCurrentUser currentUser.user
                                  window >>= location >>= setHref "/#/"
                    ]
                    [ text_ "Sign up" ]
                ]
            ]
      ]
  }
  where
  withErrors :: Array String -> Maybe String -> V (Array String) String
  withErrors e = maybe (invalid e) pure