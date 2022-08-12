module Components.Settings where

import Prelude

import API.Effects as Effects
import API.Types (User)
import Components.Common (fieldset)
import Control.Alt ((<|>))
import Control.Monad.Except (Except, throwError)
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
import Deku.Listeners (click)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (AnEvent)
import Foreign.Object (toUnfoldable)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

settings_ =
  Proxy    :: Proxy
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

settings :: forall s m lock payload. Korok s m => AnEvent m User -> (User -> Effect Unit) -> Domable m lock payload
settings currentUser setCurrentUser = settings_ ~~
  { formMatter: nut
      ( Deku.do
          setErrors /\ errors <- useState []
          setProfilePictureUrl /\ profilePictureUrl <- useState Nothing
          setName /\ name <- useState Nothing
          setBio /\ bio <- useState Nothing
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
                [ fieldset false "URL of profile picture" (Just >>> setName)
                , fieldset false "Your Name" (Just >>> setName)
                , fieldset false "Short bio about you" (Just >>> setName)
                , fieldset false "Email" (Just >>> setEmail)
                , fieldset true "Password" (Just >>> setPassword)
                , D.button
                    ( oneOf
                        [ pure $ D.Class := "btn btn-lg btn-primary pull-xs-right"
                        , click $
                            ( {currentUser: _, user: _} <$> currentUser <*> ({ email: _, password: _, username: _, image: _, bio: _ }
                                <$> email
                                <*> password
                                <*> name
                                <*> profilePictureUrl
                                <*> bio)
                            ) <#> \{user, currentUser: cu} -> do
                              launchAff_ do
                                  resp <- Effects.updateUser cu.token { user }
                                  liftEffect case resp of
                                    Left { errors: errs } -> setErrors (map (\(a /\ b) -> a <> " " <> intercalate ", " b) (toUnfoldable errs))
                                    Right u -> do
                                      setCurrentUser u.user
                                      window >>= location >>= setHref "/#/"
                        ]
                    )
                    [ text_ "Update Settings" ]
                ]
            ]
      )
  }
  where
  withErrors :: Array String -> Maybe String -> Except (Array String) String
  withErrors e = maybe (throwError e) pure

  {-
  <div class="settings-page">
    <div class="container page">
        <div class="row">

            <div class="col-md-6 offset-md-3 col-xs-12">
                <h1 class="text-xs-center">Your Settings</h1>

                <form>
                    <fieldset>
                        <fieldset class="form-group">
                            <input class="form-control" type="text" placeholder="URL of profile picture" />
                        </fieldset>
                        <fieldset class="form-group">
                            <input class="form-control form-control-lg" type="text" placeholder="Your Name" />
                        </fieldset>
                        <fieldset class="form-group">
                            <textarea class="form-control form-control-lg" rows="8"
                                      placeholder="Short bio about you"></textarea>
                        </fieldset>
                        <fieldset class="form-group">
                            <input class="form-control form-control-lg" type="text" placeholder="Email" />
                        </fieldset>
                        <fieldset class="form-group">
                            <input class="form-control form-control-lg" type="password" placeholder="Password" />
                        </fieldset>
                        <button class="btn btn-lg btn-primary pull-xs-right">
                            Update Settings
                        </button>
                    </fieldset>
                </form>
            </div>

        </div>
    </div>
</div>
  -}