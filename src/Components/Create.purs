module Components.Create where

import Prelude

import API.Effects (createArticle)
import API.Types (User)
import Components.Field (largeTextField, textField)
import Control.Alt ((<|>))
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Utils (words)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, toEither)
import Deku.Attribute ((:=))
import Deku.Control (blank, text_, (<#~>))
import Deku.Core (Domable, fixed)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Listeners (click, injectElementT)
import Deku.Pursx ((~~))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import Foreign.Object (toUnfoldable)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.HTMLTextAreaElement (value)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

create_ =
  Proxy
    :: Proxy
         """<div class="editor-page">
    <div class="container page">
        <div class="row">

            <div class="col-md-10 offset-md-1 col-xs-12">
            ~formMatter~
            </div>
            </div>
            </div>
            </div>
"""

create :: forall lock payload. Event User -> Domable lock payload
create user =
  create_ ~~
    { formMatter: fixed
        [ Deku.do
            setErrors /\ errors <- useState []
            setTitle /\ title <- useState Nothing
            setDescription /\ description <- useState Nothing
            setBodyFocus /\ bodyFocus <- useState Nothing
            setBodyElt /\ bodyElt <- useState'
            setTags /\ tags <- useState []
            let errorMessages = ((title <|> description <|> (bodyFocus $> Nothing) <|> (tags $> Nothing) <|> pure Nothing) $> []) <|> errors
            D.div_
              [D.div_ [errorMessages <#~> case _ of
                  [] -> blank
                  errs -> D.ul (oneOf [ pure $ D.Class := "error-messages" ])
                    (map (D.li_ <<< pure <<< text_) errs)]
              , D.div_
                  [ largeTextField "Article Title" (Just >>> setTitle)
                  , textField "What's this article about?" (Just >>> setDescription)
                  , D.fieldset (oneOf [ pure $ D.Class := "form-group" ])
                      [ D.textarea
                          ( oneOf
                              [ pure $ D.Class := "form-control"
                              , pure $ D.Rows := "8"
                              , injectElementT setBodyElt
                              , pure $ D.Placeholder := "Write your article (in markdown)"
                              , pure $ D.OnFocus := (setBodyFocus $ Just unit)
                              ]
                          )
                          []
                      ]
                  , textField "Enter tags" (words >>> setTags)
                  , D.button
                      ( oneOf
                          [ pure $ D.Class := "btn btn-lg btn-primary pull-xs-right"
                          , click $
                              ( { title: _, description: _, tags: _, bodyElt: _, user: _ }
                                  <$> title
                                  <*> description
                                  <*> tags
                                  <*> bodyElt
                                  <*> user
                              ) <#> \fields -> do
                                txt <- value fields.bodyElt
                                let
                                  parsed = { title: _, description: _, body: _, tagList: _ }
                                    <$> withErrors [ "Title cannot be empty" ] fields.title
                                    <*> withErrors [ "Description cannot be empty" ] fields.description
                                    <*> withErrors [ "Body cannot be empty" ] (if txt == "" then Nothing else Just txt)
                                    <*> pure fields.tags
                                case toEither parsed of
                                  Left errs -> setErrors errs
                                  Right article -> launchAff_ do
                                    resp <- createArticle fields.user.token { article }
                                    liftEffect case resp of
                                      Left { errors: errs } -> setErrors (map (\(a /\ b) -> a <> " " <> intercalate ", " b) (toUnfoldable errs))
                                      -- for now don't do anything
                                      Right _ -> do
                                        window >>= location >>= setHref "/#/"
                          ]
                      )
                      [ text_ "Publish Article" ]
                  ]
              ]
        ]
    }
  where
  withErrors :: Array String -> Maybe String -> V (Array String) String
  withErrors e = maybe (invalid e) pure