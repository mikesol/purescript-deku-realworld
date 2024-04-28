module Components.Home where

import Prelude

import API.Effects (getArticleFeed, getArticles, getArticlesWithTag)
import API.Types (Article, AuthState(..), MultipleArticles)
import Components.Favorited (doFavoriting)
import Control.Alt ((<|>))
import Deku.DOM.Combinators (runOn, runOn_)
import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.DOM.Attributes as DA
import Deku.Control (text, text_)
import Deku.Hooks ((<#~>), useState, useState')
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.DOM.Listeners as DL
import Deku.Pursx (pursx)
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
  let fc = fixed [ text (show <$> favoritesCount) ]
  let
    signedOutButton = oneOf
      [ DA.klass_ "text-success btn-sm pull-xs-right"
      , DA.style $ currentUser <#> \cu -> case cu of
          SignedIn _ -> "display:none;"
          SignedOut -> ""
      ]
  let
    signedInButton = oneOf
      [ DA.klass_ "btn btn-outline-primary btn-sm pull-xs-right"
      , DA.style $ currentUser <#> \cu -> case cu of
          SignedIn _ -> ""
          SignedOut -> "display:none;"
      , doFavoriting currentUser slug isFavorited favoritesCount setFavoritesCount setFavorited
      ]
  pursx @ArticlePreview
    { image:  DA.src_ image
    , profile1: DA.href_ $ "/#/profile/" <> username
    , profile2:  DA.href_ $ "/#/profile/" <> username
    , signedOutButton
    , signedInButton
    , href:  DA.href_ $ "/#/article/" <> slug
    , username: fixed [ text_ username ]
    , title: fixed [ D.h1_ [ text_ title ] ]
    , description: fixed [ D.p_ [ text_ description ] ]
    , date: fixed [ text_ (prettyDate updatedAt) ]
    , favoritesCount1: fc
    , favoritesCount2: fc
    }

type ArticlesLoading =
  """
                <div class="article-preview">
                        <h2>Loading...</h2>
                </div>
"""

type ArticlePreview =
  """
                <div class="article-preview">
                    <div class="article-meta">
                        <a ~profile1~ ><img ~image~ /></a>
                        <div class="info">
                            <a ~profile2~ class="author">~username~</a>
                            <span class="date">~date~</span>
                        </div>
                    <div ~signedOutButton~>
                        <i class="ion-heart"></i> ~favoritesCount1~
                    </div>
                    <button ~signedInButton~>
                        <i class="ion-heart"></i> ~favoritesCount2~
                    </button>
                    </div>
                    <a ~href~ class="preview-link">
                        ~title~
                        ~description~
                        <span>Read more...</span>
                    </a>
                </div>
"""

type Home = """<div class="home-page">

    <div class="banner">
        <div class="container">
            <h1 class="logo-font">conduit</h1>
            <p>A place to share your knowledge.</p>
        </div>
    </div>

    <div class="container page">
        <div class="row">

            <div class="col-md-9">
                <div class="feed-toggle">
                    <ul class="nav nav-pills outline-active">
                        <li class="nav-item">
                            <a ~feedAttributes~ >Your Feed</a>
                        </li>
                        <li class="nav-item">
                            <a ~globalAttributes~ >Global Feed</a>
                        </li>
                    </ul>
                </div>

                ~articlePreviews~

            </div>

            <div class="col-md-3">
                <div class="sidebar">
                    <p>Popular Tags</p>

                    ~tags~
                </div>
            </div>

        </div>
    </div>

</div>
"""

data Tab = Global | Feed

home :: Poll AuthState -> Poll ArticleLoadStatus -> Poll TagsLoadStatus -> Nut
home currentUser articleLoadStatus tagsLoadStatus = Deku.do
  setArticles /\ articles <- useState'
  setTab /\ tab <- useState Global
  pursx @Home
    { articlePreviews: fixed
        [ D.div_
            [ (articleLoadStatus <|> articles) <#~> case _ of
                ArticlesLoading -> loading
                ArticlesLoaded a -> D.div_ (map (articlePreview currentUser) a.articles)
            ]
        ]
    , feedAttributes: oneOf
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
    , globalAttributes: oneOf
        [ DA.klass $ tab <#> \ct -> "nav-link" <> case ct of
            Feed -> ""
            Global -> " active"
        , DA.style_ "cursor: pointer;"
        , runOn_ DL.click $ setArticles ArticlesLoading *> launchAff_
            do
              liftEffect $ setTab Global
              getArticles >>= liftEffect <<< setArticles <<< ArticlesLoaded
        ]
    , tags: fixed
        [ D.div_
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
    }
  where
  loading :: Nut
  loading = pursx @ArticlesLoading {}