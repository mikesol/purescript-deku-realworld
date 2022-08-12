module Components.Article where

import Prelude

import API.Types (SingleArticle)
import Deku.Attribute ((:=))
import Deku.Control (text_)
import Deku.Core (class Korok, Domable, Nut)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Type.Proxy (Proxy(..))

article_ =
  Proxy :: Proxy """<div class="article-page">

    <div class="banner">
        <div class="container">

            ~title~

            <div class="article-meta">
                <a href=""><img ~image1~ /></a>
                <div class="info">
                    <a href="" class="author">~author1~</a>
                    <span class="date">January 20th</span>
                </div>
                <button class="btn btn-sm btn-outline-secondary">
                    <i class="ion-plus-round"></i>
                    &nbsp;
                    Follow ~author2~
                </button>
                &nbsp;&nbsp;
                <button class="btn btn-sm btn-outline-primary">
                    <i class="ion-heart"></i>
                    &nbsp;
                    Favorite Post <span class="counter">(~favoritesCount1~)</span>
                </button>
            </div>

        </div>
    </div>

    <div class="container page">

        <div class="row article-content">
            <div class="col-md-12">
                ~description~
                ~articleHeader~
                ~body~
            </div>
        </div>

        <hr />

        <div class="article-actions">
            <div class="article-meta">
                <a href="profile.html"><img ~image3~ /></a>
                <div class="info">
                    <a href="" class="author">~author3~</a>
                    <span class="date">January 20th</span>
                </div>

                <button class="btn btn-sm btn-outline-secondary">
                    <i class="ion-plus-round"></i>
                    &nbsp;
                    Follow ~author4~
                </button>
                &nbsp;
                <button class="btn btn-sm btn-outline-primary">
                    <i class="ion-heart"></i>
                    &nbsp;
                    Favorite Post <span class="counter">(~favoritesCount2~)</span>
                </button>
            </div>
        </div>

        <div class="row">

            <div class="col-xs-12 col-md-8 offset-md-2">

                <form class="card comment-form">
                    <div class="card-block">
                        <textarea class="form-control" placeholder="Write a comment..." rows="3"></textarea>
                    </div>
                    <div class="card-footer">
                        <img ~image4~ class="comment-author-img"/>
                        <button class="btn btn-sm btn-primary">
                            Post Comment
                        </button>
                    </div>
                </form>

                <div class="card">
                    <div class="card-block">
                        <p class="card-text">With supporting text below as a natural lead-in to additional content.</p>
                    </div>
                    <div class="card-footer">
                        <a href="" class="comment-author">
                            <img ~image5~ class="comment-author-img"/>
                        </a>
                        &nbsp;
                        <a href="" class="comment-author">Jacob Schmidt</a>
                        <span class="date-posted">Dec 29th</span>
                    </div>
                </div>

                <div class="card">
                    <div class="card-block">
                        <p class="card-text">With supporting text below as a natural lead-in to additional content.</p>
                    </div>
                    <div class="card-footer">
                        <a href="" class="comment-author">
                            <img ~image2~ class="comment-author-img"/>
                        </a>
                        &nbsp;
                        <a href="" class="comment-author">Jacob Schmidt</a>
                        <span class="date-posted">Dec 29th</span>
                        <span class="mod-options">
              <i class="ion-edit"></i>
              <i class="ion-trash-a"></i>
            </span>
                    </div>
                </div>

            </div>

        </div>

    </div>

</div>
"""

article :: forall s m lock payload. Korok s m => SingleArticle -> Domable m lock payload
article
  { article:
      { title
      , favoritesCount
      , description
      , body
      , author:
        { username
        , image
        }
      }
  } = article_ ~~
  { title: nut (D.h1_ [ text_ title ])
  , image1: img
  , image2: img
  , image3: img
  , image4: img
  , image5: img
  , body: nut (D.p_ [text_ body ])
  , description: nut (D.p_ [text_ description ])
  , author1: authorName
  , author2: authorName
  , author3: authorName
  , author4: authorName
  , articleHeader: nut (D.h2_ [text_ title])
  , favoritesCount1: fCount
  , favoritesCount2: fCount
  }
  where
  img = pure (D.Src := image)
  authorName = nut (text_ username)
  fCount = nut (text_ (show favoritesCount))