module Components.Home where

import Prelude

import API.Types (Article, MultipleArticles)
import Date (prettyDate)
import Deku.Attribute ((:=))
import Deku.Control (blank, switcher, text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import FRP.Event (AnEvent, Event, fromEvent)
import Type.Proxy (Proxy(..))

data ArticleLoadStatus = ArticlesLoading | ArticlesLoaded MultipleArticles

articlePreview :: forall s m lock payload. Korok s m => Article -> Domable m lock payload
articlePreview
  { updatedAt
  , favoritesCount
  , title
  , description
  , author: { image, username }
  } = articlePreview_ ~~
  { image: pure (D.Src := image)
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
                    <a href="" class="preview-link">
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

home  :: forall s m lock payload. Korok s m => Event ArticleLoadStatus -> Domable m lock payload
home articleLoadStatus = home_ ~~
  { articlePreviews: nut (
    fromEvent articleLoadStatus # switcher case _ of
      ArticlesLoading -> blank
      ArticlesLoaded articles -> D.div_ (map articlePreview articles.articles)  )
  }

  {-{
    "slug": "how-to-train-your-dragon",
    "title": "How to train your dragon",
    "description": "Ever wonder how?",
    "body": "It takes a Jacobian",
    "tagList": ["dragons", "training"],
    "createdAt": "2016-02-18T03:22:56.637Z",
    "updatedAt": "2016-02-18T03:48:35.824Z",
    "favorited": false,
    "favoritesCount": 0,
    "author": {
      "username": "jake",
      "bio": "I work at statefarm",
      "image": "https://i.stack.imgur.com/xHWG8.jpg",
      "following": false
    }
  }-}