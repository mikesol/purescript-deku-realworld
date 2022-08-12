module Components.Home where

import Prelude

import API.Effects (getArticleFeed, getArticles, getArticlesWithTag)
import API.Types (Article, MultipleArticles, User)
import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.Attribute ((:=))
import Deku.Control (blank, switcher, text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Do (useMemoized, useState, useState')
import Deku.Do as Deku
import Deku.Listeners (click)
import Deku.Pursx (nut, (~~))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event, fromEvent)
import Type.Proxy (Proxy(..))

data ArticleLoadStatus = ArticlesLoading | ArticlesLoaded MultipleArticles
data TagsLoadStatus = TagsLoading | TagsLoaded { tags :: Array String }

articlePreview :: forall s m lock payload. Korok s m => Article -> Domable m lock payload
articlePreview
  { updatedAt
  , favoritesCount
  , title
  , description
  , slug
  , author: { image, username }
  } = articlePreview_ ~~
  { image: pure (D.Src := image)
  , href: pure (D.Href := "/#/article/" <> slug)
  , username: nut (text_ username)
  , title: nut (D.h1_ [ text_ title ])
  , description: nut (D.p_ [ text_ description ])
  , date: nut (text_ (prettyDate updatedAt))
  , favoritesCount: nut (text_ (show favoritesCount))
  }

articlePreview_ =
  Proxy    :: Proxy
         """
                <div class="article-preview">
                    <div class="article-meta">
                        <a href="profile.html"><img ~image~ /></a>
                        <div class="info">
                            <a href="" class="author">~username~</a>
                            <span class="date">~date~</span>
                        </div>
                        <button class="btn btn-outline-primary btn-sm pull-xs-right">
                            <i class="ion-heart"></i> ~favoritesCount~
                        </button>
                    </div>
                    <a ~href~ class="preview-link">
                        ~title~
                        ~description~
                        <span>Read more...</span>
                    </a>
                </div>
"""

home_ =
  Proxy    :: Proxy
         """<div class="home-page">

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

home :: forall s m lock payload. Korok s m => Event (Maybe User) -> Event ArticleLoadStatus -> Event TagsLoadStatus -> Domable m lock payload
home currentUser articleLoadStatus tagsLoadStatus = Deku.do
  setArticles /\ articles <- useState'
  setTab /\ tab <- useMemoized (_ <|> pure Global)
  home_ ~~
    { articlePreviews: nut
        ( (fromEvent articleLoadStatus <|> articles) # switcher case _ of
            ArticlesLoading -> blank
            ArticlesLoaded a -> D.div_ (map articlePreview a.articles)
        )
    , feedAttributes: oneOf
        [ { cu: _, ct: _ } <$> (fromEvent currentUser) <*> tab <#> \{ cu, ct } -> D.Class := "nav-link"
            <>
              ( case cu of
                  Just _ -> ""
                  Nothing -> " disabled"
              )
            <>
              ( case ct of
                  Feed -> " active"
                  Global -> ""
              )
        , fromEvent currentUser <#> \cu -> D.Style := case cu of
            Nothing -> ""
            Just _ -> "cursor: pointer;"
        , click $ fromEvent currentUser <#> case _ of
            Nothing -> pure unit
            Just cu -> launchAff_
              do
                liftEffect $ setTab Feed
                getArticleFeed cu.token >>= liftEffect <<< setArticles <<< ArticlesLoaded
        ]
    , globalAttributes: oneOf
        [ tab <#> \ct -> D.Class := "nav-link" <> case ct of
            Feed -> ""
            Global -> " active"
        , pure $ D.Style := "cursor: pointer;"
        , click $ pure $ launchAff_
            do
              liftEffect $ setTab Global
              getArticles >>= liftEffect <<< setArticles <<< ArticlesLoaded
        ]
    , tags: nut
        ( fromEvent tagsLoadStatus # switcher case _ of
            TagsLoading -> blank
            TagsLoaded tags -> D.div (oneOf [ pure $ D.Class := "tag-list" ])
              ( map
                  ( \tag -> D.a
                      ( oneOf
                          [ pure $ D.Class := "tag-pill tag-default"
                          , pure $ D.Style := "cursor: pointer;"
                          , click $ pure $ launchAff_
                              do
                                getArticlesWithTag tag >>= liftEffect <<< setArticles <<< ArticlesLoaded
                          ]
                      )
                      [ text_ tag ]
                  )
                  tags.tags
              )
        )
    }