module Components.Article where

import Prelude

import API.Effects (addComment, deleteComment)
import API.Types (AuthState(..), Comment, SingleArticle, isSignedIn, whenSignedIn)
import Components.Favorited (doFavoriting)
import Components.Following (followAttrs, followText)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (oneOf, oneOfMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.Control (text, text_)
import Deku.Core (Nut, fixed, useDynAtBeginning)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Combinators (injectElementT, runOn, runOn_)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useHot, useState, useState', (<#~>))
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import FRP.Poll (Poll)
import Record (union)
import Web.HTML.HTMLTextAreaElement (value)

data ArticleStatus = ArticleLoading | ArticleLoaded SingleArticle (Array Comment)

data CommentText = CommentText String | NoText

derive instance Eq CommentText

makeComment
  :: { body :: String
     , date :: String
     , deleteAction :: Maybe _
     , imgSrc :: String
     , profile :: _
     , username :: String
     }
  -> Nut
makeComment { body, profile, date, deleteAction, username, imgSrc } = D.div [ DA.klass_ "card" ]
  [ D.div [ DA.klass_ "card-block" ]
      [ D.p [ DA.klass_ "card-text" ]
          [ text_ body ]
      ]
  , D.div [ DA.klass_ "card-footer" ]
      ( [ D.a
            [ profile, DA.klass_ "comment-author" ]
            [ D.img [ DA.klass_ "comment-author-img", DA.src_ imgSrc ] []
            ]
        , text_ " "
        , D.a
            [ profile, DA.klass_ "comment-author" ]
            [ text_ username ]
        , D.span [ DA.klass_ "date-posted" ]
            [ text_ date
            ]
        ] <> maybe []
          ( \da ->
              [ D.span [ DA.klass_ "mod-options" ]
                  [
                    -- deleteAction
                    D.i [ da, DA.klass_ "ion-trash-a" ] []
                  ]
              ]
          )
          deleteAction
      )
  ]

makeArticle :: Poll AuthState -> Poll ArticleStatus -> Nut
makeArticle e astat = astat <#~> case _ of
  ArticleLoaded a cmt -> articleLoaded e a cmt
  ArticleLoading -> D.div [ DA.klass_ "article-page" ]
    [ D.div [ DA.klass_ "banner" ]
        [ D.div [ DA.klass_ "container" ]
            [ D.h1 [] [ text_ "Loading..." ]
            ]
        ]
    ]

articleLoaded :: Poll AuthState -> SingleArticle -> Array Comment -> Nut
articleLoaded
  currentUser
  { article:
      { title
      , slug
      , updatedAt
      , favoritesCount: favC
      , description
      , body
      , favorited
      , author:
          { username
          , image
          , following
          }
      }
  }
  comments = Deku.do
  setFollowing /\ isFollowing <- useState following
  setFavorited /\ isFavorited <- useState favorited
  setNewComment /\ newComment <- useState'
  setCommentTA /\ commentTA <- useState'
  setFavoritesCount /\ favoritesCount <- useHot favC
  let followAttrs' = followAttrs username currentUser isFollowing setFollowing
  let followText' = followText isFollowing
  let
    favoriteAttrs = oneOf
      [ DA.klass_ "btn btn-sm btn-outline-primary"
      , DA.style $ currentUser <#> \cu -> if isSignedIn cu then "" else "display: none;"
      , doFavoriting currentUser slug isFavorited favoritesCount setFavoritesCount setFavorited
      ]
  let favoriteText = fixed [ text (isFavorited <#> if _ then "Favorited" else "Favorite Post") ]
  let img = DA.src_ image
  let
    myImg = DA.src $ currentUser <#>
      ( fromMaybe "https://picsum.photos/200"
          <<< case _ of
            SignedIn u -> u.image
            SignedOut -> Nothing
      )
  let authProf = DA.href_ $ "/#/profile/" <> username
  let authorName = text_ username
  let fCount = text (show <$> favoritesCount)
  D.div [ DA.klass_ "article-page" ]
    [ D.div [ DA.klass_ "banner" ]
        [ D.div [ DA.klass_ "container" ]
            [
              -- title
              D.h1_ [ text_ title ]
            , D.div [ DA.klass_ "article-meta" ]
                [ D.a
                    [ authProf ]
                    [ D.img [ img ] []
                    ]
                , D.div [ DA.klass_ "info" ]
                    [ D.a
                        [ authProf, DA.klass_ "author" ]
                        [ authorName ]
                    , D.span [ DA.klass_ "date" ]
                        [ text_ (prettyDate updatedAt)
                        ]
                    ]
                , D.button followAttrs'
                    [ D.i [ DA.klass_ "ion-plus-round" ] []
                    , text_ " "
                    , followText'
                    , authorName
                    ]
                , text_ "  "
                , D.button
                    [ favoriteAttrs ]
                    [ D.i [ DA.klass_ "ion-heart" ] []
                    , text_ " "
                    , favoriteText
                    , D.span [ DA.klass_ "counter" ] [ text_ "(", fCount, text_ ")" ]
                    ]
                ]
            ]
        ]
    , D.div [ DA.klass_ "container page" ]
        [ D.div [ DA.klass_ "row article-content" ]
            [ D.div [ DA.klass_ "col-md-12" ]
                [ D.p_ [ text_ description ]
                , D.h2_ [ text_ title ]
                , D.p_ [ text_ body ]
                ]
            ]
        , D.hr_ []
        , D.div [ DA.klass_ "article-actions" ]
            [ D.div [ DA.klass_ "article-meta" ]
                [ D.a [ authProf ]
                    [ D.img [ img ] []
                    ]
                , D.div [ DA.klass_ "info" ]
                    [ D.a [ authProf, DA.klass_ "author" ] [ authorName ]
                    , D.span [ DA.klass_ "date" ] [ text_ "January 20th" ]
                    ]
                , D.button followAttrs'
                    [ D.i [ DA.klass_ "ion-plus-round" ] []
                    , text_ " "
                    , followText'
                    , authorName
                    ]
                , text_ " "
                , D.button
                    [ favoriteAttrs ]
                    [ D.i [ DA.klass_ "ion-heart" ] []
                    , text_ " "
                    , favoriteText
                    , D.span [ DA.klass_ "counter" ] [ text_ "(", fCount, text_ ")" ]
                    ]
                ]
            ]
        , D.div [ DA.klass_ "row" ]
            [ D.div [ DA.klass_ "col-xs-12 col-md-8 offset-md-2" ]
                [ D.div
                    [ DA.style $ currentUser <#> \cu -> if isSignedIn cu then "" else "display: none;", DA.klass_ "card comment-form" ]
                    [ D.div [ DA.klass_ "card-block" ]
                        [ D.textarea
                            [ injectElementT setCommentTA
                            , DA.klass_ "form-control"
                            , DA.placeholder_ "Write a comment..."
                            , DA.rows_ "3"
                            ]
                            []
                        ]
                    , D.div [ DA.klass_ "card-footer" ]
                        [ D.img [ DA.klass_ "comment-author-img", myImg ] []
                        , D.button
                            [ DA.klass_ "btn btn-sm btn-primary"
                            , runOn DL.click $ ({ cu: _, ta: _ } <$> currentUser <*> commentTA) <#> \{ cu, ta } -> do
                                v <- value ta
                                whenSignedIn cu \cu' -> do
                                  launchAff_ $ do
                                    addComment cu'.token slug v >>= case _ of
                                      Right c -> liftEffect $ setNewComment c.comment
                                      Left e -> throwError (error (show e))
                            ]
                            [ text_ "Post Comment" ]
                        ]
                    ]
                , D.div_
                    [ Deku.do
                        { value: { cu, com }, remove } <- useDynAtBeginning (({ cu: _, com: _ }) <$> currentUser <*> (newComment <|> oneOfMap pure comments))
                        let profile = DA.href_ $ "/#/profile/" <> com.author.username
                        let
                          common =
                            { body: com.body
                            , imgSrc: com.author.image
                            , profile
                            , username: com.author.username
                            , date: prettyDate com.updatedAt
                            }
                        maybe (makeComment $ common `union` { deleteAction: Nothing })
                          ( \u -> do
                              let
                                deleteAction = runOn_ DL.click do
                                  launchAff_ $ deleteComment u.token slug com.id
                                  remove
                              makeComment $ common `union` { deleteAction: Just deleteAction }
                          )
                          ( case cu of
                              SignedIn u
                                | u.username == com.author.username -> Just u
                                | otherwise -> Nothing
                              SignedOut -> Nothing
                          )
                    ]
                ]
            ]
        ]
    ]
