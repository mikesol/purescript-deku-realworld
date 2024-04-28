module Components.Home where

import Prelude

import API.Effects (getArticleFeed, getArticles, getArticlesWithTag)
import API.Types (Article, AuthState(..), MultipleArticles)
import Components.Favorited (doFavoriting)
import Control.Alt ((<|>))
import Deku.DOM.Combinators (runOn, runOn_)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.DOM.Attributes as DA
import Deku.Control (text, text_)
import Deku.Hooks ((<#~>), useState, useState')
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.DOM.Listeners as DL
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Poll (Poll)

data ArticleLoadStatus = ArticlesLoading | ArticlesLoaded MultipleArticles
data TagsLoadStatus = TagsLoading | TagsLoaded { tags :: Array String }

articlePreview :: Poll AuthState -> Article -> Nut
articlePreview
  currentUser
  { updatedAt
  , favoritesCount: fcount
  , title
  , description
  , slug
  , favorited
  , author: { image, username }
  } = Deku.do
  setFavoritesCount /\ favoritesCount <- useState fcount
  setFavorited /\ isFavorited <- useState favorited
  let
    fc = text (show <$> favoritesCount)
    profileHref = DA.href_ $ "/#/profile/" <> username
  D.div [ DA.klass_ "article-preview" ]
    [ D.div [ DA.klass_ "article-meta" ]
        [ D.a
            [ profileHref ]
            [ D.img
                [ DA.src_ image ]
                []
            ]
        , D.div [ DA.klass_ "info" ]
            [ D.a
                [ profileHref, DA.klass_ "author" ]
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
        [ DA.href_ $ "/#/article/" <> slug, DA.klass_ "preview-link" ]
        [ D.h1_ [ text_ title ]
        , D.p_ [ text_ description ]
        , D.span [] [ text_ "Read more..." ]
        ]
    ]

data Tab = Global | Feed

home :: Poll AuthState -> Poll ArticleLoadStatus -> Poll TagsLoadStatus -> Nut
home currentUser articleLoadStatus tagsLoadStatus = Deku.do
  setArticles /\ articles <- useState'
  setTab /\ tab <- useState Global
  D.div [ DA.klass_ "home-page" ]
    [ D.div [ DA.klass_ "banner" ]
        [ D.div [ DA.klass_ "container" ]
            [ D.h1 [ DA.klass_ "logo-font" ] [ text_ "conduit" ]
            , D.p [] [ text_ "A place to share your knowledge." ]
            ]
        ]
    , D.div [ DA.klass_ "container page" ]
        [ D.div [ DA.klass_ "row" ]
            [ D.div [ DA.klass_ "col-md-9" ]
                [ D.div [ DA.klass_ "feed-toggle" ]
                    [ D.ul [ DA.klass_ "nav nav-pills outline-active" ]
                        [ D.li [ DA.klass_ "nav-item" ]
                            [ D.a
                                [ DA.klass $ { cu: _, ct: _ } <$> currentUser <*> tab <#> \{ cu, ct } -> "nav-link"
                                    <>
                                      ( case cu of
                                          SignedIn _ -> ""
                                          SignedOut -> " disabled"
                                      )
                                    <>
                                      ( case ct of
                                          Feed -> " active"
                                          Global -> ""
                                      )
                                , DA.style $ currentUser <#> \cu -> case cu of
                                    SignedOut -> ""
                                    SignedIn _ -> "cursor: pointer;"
                                , runOn DL.click $ currentUser <#> case _ of
                                    SignedOut -> pure unit
                                    SignedIn cu -> setArticles ArticlesLoading *> launchAff_
                                      do
                                        liftEffect $ setTab Feed
                                        getArticleFeed cu.token >>= liftEffect <<< setArticles <<< ArticlesLoaded
                                ]
                                [ text_ "Your Feed" ]
                            ]
                        , D.li [ DA.klass_ "nav-item" ]
                            [ D.a
                                [ DA.klass $ tab <#> \ct -> "nav-link" <> case ct of
                                    Feed -> ""
                                    Global -> " active"
                                , DA.style_ "cursor: pointer;"
                                , runOn_ DL.click $ setArticles ArticlesLoading *> launchAff_
                                    do
                                      liftEffect $ setTab Global
                                      getArticles >>= liftEffect <<< setArticles <<< ArticlesLoaded
                                ]
                                [ text_ "Global Feed" ]
                            ]
                        ]
                    ]
                , D.div_
                    [ (articleLoadStatus <|> articles) <#~> case _ of
                        ArticlesLoading -> loading
                        ArticlesLoaded a -> D.div_ (map (articlePreview currentUser) a.articles)
                    ]
                ]
            , D.div [ DA.klass_ "col-md-3" ]
                [ D.div [ DA.klass_ "sidebar" ]
                    [ D.p [] [ text_ "Popular Tags" ]
                    , D.div_
                        [ tagsLoadStatus <#~> case _ of
                            TagsLoading -> mempty
                            TagsLoaded tags -> D.div [ DA.klass_ "tag-list" ]
                              ( map
                                  ( \tag -> D.a
                                      [ DA.klass_ "tag-pill tag-default"
                                      , DA.style_ "cursor: pointer;"
                                      , runOn_ DL.click $ setArticles ArticlesLoading *> launchAff_
                                          do
                                            getArticlesWithTag tag >>= liftEffect <<< setArticles <<< ArticlesLoaded
                                      ]

                                      [ text_ tag ]
                                  )
                                  tags.tags
                              )
                        ]
                    ]
                ]
            ]
        ]
    ]

  where
  loading :: Nut
  loading = D.div [ DA.klass_ "article-preview" ]
    [ D.h2 [] [ text_ "Loading..." ]
    ]