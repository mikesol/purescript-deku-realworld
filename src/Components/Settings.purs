module Components.Settings where

import Prelude

import API.Effects as Effects
import API.Types (User)
import Components.Field (passwordField, textFieldWithValue)
import Control.Alt ((<|>))
import Data.Array (intercalate)
import Data.Compactable (compact)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=))
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
import FRP.Event (Event)
import Foreign.Object (toUnfoldable)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

settings_ =
  Proxy
    :: Proxy
         """<div class="settings-page">
    <div class="container page">
        <div class="row">

            <div class="col-md-6 offset-md-3 col-xs-12">
                <h1 class="text-xs-center">Your Settings</h1>


                ~formMatter~
            </div>

        </div>
    </div>
</div>"""

settings :: Event User -> (User -> Effect Unit) -> Nut
settings currentUser setCurrentUser = settings_ ~~
  { formMatter: fixed
      [ Deku.do
          setErrors /\ errors <- useState []
          setProfilePictureUrl /\ profilePictureUrl <- useState Nothing
          setName /\ name <- useState Nothing
          setBio /\ bio <- useState Nothing
          setEmail /\ email <- useState Nothing
          setPassword /\ password <- useState Nothing
          let errorMessages = ((email <|> password <|> pure Nothing) $> []) <|> errors
          let onCurrentUser f = f <$> currentUser
          let onCurrentUserM f = compact (f <$> currentUser)
          D.div_
            [ D.div_ $
                [ errorMessages <#~> case _ of
                    [] -> blank
                    errs -> D.ul [ D.Class !:= "error-messages" ]
                      (map (D.li_ <<< pure <<< text_) errs)
                ]
            , D.div_
                [ textFieldWithValue (onCurrentUserM _.image) "URL of profile picture" (Just >>> setProfilePictureUrl)
                , textFieldWithValue (onCurrentUser _.username) "Your Name" (Just >>> setName)
                , textFieldWithValue (onCurrentUserM _.bio) "Short bio about you" (Just >>> setBio)
                , textFieldWithValue (onCurrentUser _.email) "Email" (Just >>> setEmail)
                , passwordField "Password" (Just >>> setPassword)
                , D.button
                    [ D.Class !:= "btn btn-lg btn-primary pull-xs-right"
                    , click $
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
  }
