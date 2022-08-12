module Components.Home where

import Prelude

import API.Types (Article, MultipleArticles, User)
import Data.Maybe (Maybe)
import Date (prettyDate)
import Deku.Attribute ((:=))
import Deku.Control (blank, switcher, text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import FRP.Event (Event, fromEvent)
import Type.Proxy (Proxy(..))

data ArticleLoadStatus = ArticlesLoading | ArticlesLoaded MultipleArticles

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
  , title: nut (D.h1_ [text_ title])
  , description: nut (D.p_ [text_ description])
  , date: nut (text_ (prettyDate updatedAt))
  , favoritesCount: nut (text_ (show favoritesCount))
  }

articlePreview_ =
  Proxy :: Proxy """
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
  Proxy :: Proxy """<div class="home-page">

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
                            <a class="nav-link disabled" href="">Your Feed</a>
                        </li>
                        <li class="nav-item">
                            <a class="nav-link active" href="">Global Feed</a>
                        </li>
                    </ul>
                </div>

                ~articlePreviews~

            </div>

            <div class="col-md-3">
                <div class="sidebar">
                    <p>Popular Tags</p>

                    <div class="tag-list">
                        <a href="" class="tag-pill tag-default">programming</a>
                        <a href="" class="tag-pill tag-default">javascript</a>
                        <a href="" class="tag-pill tag-default">emberjs</a>
                        <a href="" class="tag-pill tag-default">angularjs</a>
                        <a href="" class="tag-pill tag-default">react</a>
                        <a href="" class="tag-pill tag-default">mean</a>
                        <a href="" class="tag-pill tag-default">node</a>
                        <a href="" class="tag-pill tag-default">rails</a>
                    </div>
                </div>
            </div>

        </div>
    </div>

</div>
"""

home  :: forall s m lock payload. Korok s m => Event (Maybe User) -> Event ArticleLoadStatus -> Domable m lock payload
home userEvent articleLoadStatus = home_ ~~
  { articlePreviews: nut (
    fromEvent articleLoadStatus # switcher case _ of
      ArticlesLoading -> blank
      ArticlesLoaded articles -> D.div_ (map articlePreview articles.articles))
  }