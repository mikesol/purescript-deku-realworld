module Components.Settings where

import Prelude

import API.Effects as Effects
import Deku.DOM.Attributes as DA
import Deku.DOM.Combinators (runOn)
import API.Types (User)
import Components.Field (passwordField, textFieldWithValue)
import Control.Alt ((<|>))
import Data.Array (intercalate)
import Data.Compactable (compact)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.DOM.Listeners as DL
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Poll (Poll)
import Foreign.Object (toUnfoldable)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

settings :: Poll User -> (User -> Effect Unit) -> Nut
settings currentUser setCurrentUser = Deku.do
  setErrors /\ errors <- useState []
  setProfilePictureUrl /\ profilePictureUrl <- useState Nothing
  setName /\ name <- useState Nothing
  setBio /\ bio <- useState Nothing
  setEmail /\ email <- useState Nothing
  setPassword /\ password <- useState Nothing
  let errorMessages = ((email <|> password <|> pure Nothing) $> []) <|> errors
  let onCurrentUser f = f <$> currentUser
  let onCurrentUserM f = compact (f <$> currentUser)
  D.div [ DA.klass_ "settings-page" ]
    [ D.div [ DA.klass_ "container page" ]
        [ D.div [ DA.klass_ "row" ]
            [ D.div [ DA.klass_ "col-md-6 offset-md-3 col-xs-12" ]
                [ D.h1 [ DA.klass_ "text-xs-center" ] [ text_ "Your Settings" ]
                , D.div_
                    [ D.div_ $
                        [ errorMessages <#~> case _ of
                            [] -> mempty
                            errs -> D.ul [ DA.klass_ "error-messages" ]
                              (map (D.li_ <<< pure <<< text_) errs)
                        ]
                    , D.div_
                        [ textFieldWithValue (onCurrentUserM _.image) "URL of profile picture" (Just >>> setProfilePictureUrl)
                        , textFieldWithValue (onCurrentUser _.username) "Your Name" (Just >>> setName)
                        , textFieldWithValue (onCurrentUserM _.bio) "Short bio about you" (Just >>> setBio)
                        , textFieldWithValue (onCurrentUser _.email) "Email" (Just >>> setEmail)
                        , passwordField "Password" (Just >>> setPassword)
                        , D.button
                            [ DA.klass_ "btn btn-lg btn-primary pull-xs-right"
                            , runOn DL.click $
                                ( { currentUser: _, user: _ } <$> currentUser <*>
                                    ( { email: _, password: _, username: _, image: _, bio: _ }
                                        <$> email
                                        <*> password
                                        <*> name
                                        <*> profilePictureUrl
                                        <*> bio
                                    )
                                ) <#> \{ user, currentUser: cu } -> do
                                  launchAff_ do
                                    resp <- Effects.updateUser cu.token { user }
                                    liftEffect case resp of
                                      Left { errors: errs } -> setErrors (map (\(a /\ b) -> a <> " " <> intercalate ", " b) (toUnfoldable errs))
                                      Right u -> do
                                        setCurrentUser u.user
                                        window >>= location >>= setHref "/#/"
                            ]

                            [ text_ "Update Settings" ]
                        ]
                    ]
                ]
            ]
        ]
    ]