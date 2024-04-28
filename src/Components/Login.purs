module Components.Login where

import Prelude

import API.Effects (logIn)
import API.Types (User)
import Components.Field (largePasswordField, largeTextField)
import Control.Alt ((<|>))
import Deku.DOM.Attributes as DA
import Data.Array (intercalate)
import Data.Either (Either(..))
import Deku.DOM.Combinators (runOn)
import Deku.DOM.Listeners as DL
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, toEither)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Foreign.Object (toUnfoldable)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

login :: (User -> Effect Unit) -> Nut
login setCurrentUser = Deku.do
  setErrors /\ errors <- useState []
  setEmail /\ email <- useState Nothing
  setPassword /\ password <- useState Nothing
  let errorMessages = ((email <|> password <|> pure Nothing) $> []) <|> errors
  D.div [ DA.klass_ "auth-page" ]
    [ D.div [ DA.klass_ "container page" ]
        [ D.div [ DA.klass_ "row" ]
            [ D.div [ DA.klass_ "col-md-6 offset-md-3 col-xs-12" ]
                [ D.h1 [ DA.id_ "sign-in-h1", DA.klass_ "text-xs-center" ] [ text_ "Sign in" ]
                , D.div_
                    [ D.div_
                        [ errorMessages <#~> case _ of
                            [] -> mempty
                            errs -> D.ul [ DA.klass_ "error-messages" ]
                              (map (D.li_ <<< pure <<< text_) errs)
                        ]
                    , D.div_
                        [ largeTextField "Email" (Just >>> setEmail)
                        , largePasswordField "Password" (Just >>> setPassword)
                        , D.button
                            [ DA.klass_ "btn btn-lg btn-primary pull-xs-right"
                            , runOn DL.click $
                                ( { email: _, password: _ }
                                    <$> email
                                    <*> password
                                ) <#> \fields -> do
                                  let
                                    parsed = { email: _, password: _ }
                                      <$> withErrors [ "Email cannot be empty" ] fields.email
                                      <*> withErrors [ "Password cannot be empty" ] fields.password
                                  case toEither parsed of
                                    Left errs -> setErrors errs
                                    Right user -> launchAff_ do
                                      resp <- logIn { user }
                                      liftEffect case resp of
                                        Left { errors: errs } -> setErrors (map (\(a /\ b) -> a <> " " <> intercalate ", " b) (toUnfoldable errs))
                                        Right currentUser -> do
                                          setCurrentUser currentUser.user
                                          window >>= location >>= setHref "/#/"
                            ]
                            [ text_ "Sign in" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

  where
  withErrors :: Array String -> Maybe String -> V (Array String) String
  withErrors e = maybe (invalid e) pure