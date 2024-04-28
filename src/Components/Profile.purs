module Components.Profile where

import Prelude

import API.Types (Article, AuthState(..), MultipleArticles, SingleProfile)
import Components.Favorited (doFavoriting)
import FRP.Poll (Poll)
import Components.Following (followAttrs, followText)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Combinators (runOn_)
import Deku.DOM.Attributes as DA
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.DOM.Listeners as DL

data ProfileStatus = ProfileLoading | ProfileLoaded SingleProfile MultipleArticles MultipleArticles

singleArticle :: Poll AuthState -> Article -> Nut
singleArticle
  currentUser
  { updatedAt
  , favoritesCount: fcount
  , favorited
  , slug
  , title
  , description
  , author: { username, image }
  } = Deku.do
  setFavoritesCount /\ favoritesCount <- useState fcount
  setFavorited /\ isFavorited <- useState favorited
  let fc = text (show <$> favoritesCount)
  let authorHref = DA.href_ ("/#/profile/" <> username)
  D.div [ DA.klass_ "article-preview" ]
    [ D.div [ DA.klass_ "article-meta" ]
        [ D.a
            [ authorHref ]
            [ D.img [ DA.src_ image ] [] ]
        , D.div [ DA.klass_ "info" ]
            [ D.a
                [ authorHref, DA.klass_ "author" ]
                [ text_ username ]
            , D.span [ DA.klass_ "date" ] [ text_ (prettyDate updatedAt) ]
            ]
        , D.div
            [ DA.klass_ "text-success btn-sm pull-xs-right"
            , DA.style $ currentUser <#> \cu -> case cu of
                SignedIn _ -> "display:none;"
                SignedOut -> ""
            ]
            [ D.i [ DA.klass_ "ion-heart" ] []
            , fc
            ]
        , D.button
            [ DA.klass_ "btn btn-outline-primary btn-sm pull-xs-right"
            , DA.style $ currentUser <#> \cu -> case cu of
                SignedIn _ -> ""
                SignedOut -> "display:none;"
            , doFavoriting currentUser slug isFavorited favoritesCount setFavoritesCount setFavorited
            ]
            [ D.i [ DA.klass_ "ion-heart" ] []
            , fc
            ]
        ]
    , D.a
        [ DA.href_ ("/#/article/" <> slug), DA.klass_ "preview-link" ]
        [ D.h1_ [ text_ title ]
        , D.p_ [ text_ description ]
        , D.span [] [ text_ "Read more..." ]
        ]
    ]

makeProfile :: Poll AuthState -> Poll ProfileStatus -> Nut
makeProfile e pstat = pstat <#~> case _ of
  ProfileLoaded a b c -> profileLoaded e a b c
  ProfileLoading -> D.div [ DA.klass_ "profile-page" ]
    [ D.div [ DA.klass_ "user-info" ]
        [ D.div [ DA.klass_ "container" ]
            [ D.div [ DA.klass_ "row" ]
                [ D.div [ DA.klass_ "col-xs-12 col-md-10 offset-md-1" ]
                    [ D.h4 [] [ text_ "Loading..." ]
                    ]
                ]
            ]
        ]
    ]

data Tab = MyArticles | FavoritedArticles

profileLoaded :: Poll AuthState -> SingleProfile -> MultipleArticles -> MultipleArticles -> Nut
profileLoaded
  currentUser
  { profile:
      { username
      , image
      , bio
      , following
      }
  }
  myArticles
  favoritedArticles = Deku.do
  setFollowing /\ isFollowing <- useState following
  let followAttrs' = followAttrs username currentUser isFollowing setFollowing
  let followText' = followText isFollowing
  setTab /\ tab <- useState MyArticles
  D.div [ DA.klass_ "profile-page" ]
    [ D.div [ DA.klass_ "user-info" ]
        [ D.div [ DA.klass_ "container" ]
            [ D.div [ DA.klass_ "row" ]
                [ D.div [ DA.klass_ "col-xs-12 col-md-10 offset-md-1" ]
                    [ D.img [ DA.klass_ "user-img", DA.src_ image ]
                        []
                    , D.h4_ [ text_ username ]
                    , D.p []
                        [ maybe mempty (\b -> D.h4_ [ text_ b ]) bio
                        ]
                    , D.button ([ DA.klass_ "btn btn-sm btn-outline-secondary action-btn" ] <> followAttrs')
                        [ D.i [ DA.klass_ "ion-plus-round" ] []
                        , text_ " "
                        , followText'
                        , text_ username
                        ]
                    ]
                ]
            ]
        ]
    , D.div [ DA.klass_ "container" ]
        [ D.div [ DA.klass_ "row" ]
            [ D.div [ DA.klass_ "col-xs-12 col-md-10 offset-md-1" ]
                [ D.div [ DA.klass_ "articles-toggle" ]
                    [ D.ul [ DA.klass_ "nav nav-pills outline-active" ]
                        [ D.li [ DA.klass_ "nav-item" ]
                            [ D.a
                                [ DA.klass $ tab <#> \ct -> "nav-link" <> case ct of
                                    FavoritedArticles -> ""
                                    MyArticles -> " active"
                                , DA.style_ "cursor: pointer;"
                                , runOn_ DL.click (setTab MyArticles)
                                , DA.klass_ "nav-link active"
                                ]
                                [ text_ "My Articles" ]
                            ]
                        , D.li [ DA.klass_ "nav-item" ]
                            [ D.a 
                                [ DA.klass $ tab <#> \ct -> "nav-link" <> case ct of
                                      FavoritedArticles -> " active"
                                      MyArticles -> ""
                                  , DA.style_ "cursor: pointer;"
                                  , runOn_ DL.click (setTab FavoritedArticles)
                                  ,DA.klass_ "nav-link" ]
                                
                                [ text_ "Favorited Articles" ]
                            ]
                        ]
                    ]
                , let
                    su = singleArticle currentUser
                  in
                    D.div_
                      [ tab <#~> case _ of
                          FavoritedArticles -> D.div_ (map su favoritedArticles.articles)
                          MyArticles -> D.div_ (map su myArticles.articles)
                      ]
                ]
            ]
        ]
    ]
