module Components.Article where

import Prelude

import API.Effects (favorite, follow, unfavorite, unfollow)
import API.Types (SingleArticle, User)
import Control.Alt ((<|>))
import Data.Foldable (for_, oneOf)
import Data.Maybe (Maybe, isJust)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (text, text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Do (useMemoized)
import Deku.Do as Deku
import Deku.Listeners (click)
import Deku.Pursx (nut, (~~))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Dedup (dedup)
import FRP.Event (AnEvent)
import Type.Proxy (Proxy(..))

article_ =
  Proxy    :: Proxy
         """<div class="article-page">

    <div class="banner">
        <div class="container">

            ~title~

            <div class="article-meta">
                <a href=""><img ~image1~ /></a>
                <div class="info">
                    <a href="" class="author">~author1~</a>
                    <span class="date">January 20th</span>
                </div>
                <button ~followAttrs1~ >
                    <i class="ion-plus-round"></i>
                    &nbsp;
                    ~followText1~ ~author2~
                </button>
                &nbsp;&nbsp;
                <button ~favoriteAttrs1~ >
                    <i class="ion-heart"></i>
                    &nbsp;
                    ~favoriteText1~ <span class="counter">(~favoritesCount1~)</span>
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

                <button ~followAttrs2~ >
                    <i class="ion-plus-round"></i>
                    &nbsp;
                    ~followText2~ ~author4~
                </button>
                &nbsp;
                <button ~favoriteAttrs2~ >
                    <i class="ion-heart"></i>
                    &nbsp;
                    ~favoriteText2~ <span class="counter">(~favoritesCount2~)</span>
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

article :: forall s m lock payload. Korok s m => AnEvent m (Maybe User) -> SingleArticle -> Domable m lock payload
article
  currentUser
  { article:
      { title
      , slug
      , favoritesCount
      , description
      , body
      , favorited
      , author:
          { username
          , image
          , following
          }
      }
  } = Deku.do
  setFollowing /\ isFollowing <- useMemoized (_ <|> pure following)
  setFavorited /\ isFavorited <- useMemoized (_ <|> pure favorited)
  setFavoritesCount /\ favoritesCount <- useMemoized ((_ <|> pure favoritesCount) >>> dedup)
  let
    followAttrs = oneOf
      [ currentUser <#> \cu -> D.Class := ("btn btn-sm btn-outline-secondary" <> if isJust cu then "" else " disabled")
      , click $ ({ cu: _, flw: _ } <$> currentUser <*> isFollowing) <#> \{ cu, flw } -> do
          for_ cu \cu' -> do
            setFollowing (not flw)
            launchAff_ do
              if flw then
                void $ unfollow cu'.token username
              else
                void $ follow cu'.token username
      ]
  let followText = nut (text (isFollowing <#> if _ then "Following" else "Follow"))
  let
    favoriteAttrs = oneOf
      [ currentUser <#> \cu -> D.Class := ("btn btn-sm btn-outline-primary" <> if isJust cu then "" else " disabled")
      , click $ ({ cu: _, fv: _, fc: _ } <$> currentUser <*> isFavorited <*> favoritesCount) <#> \{ cu, fv, fc } -> do
          for_ cu \cu' -> do
            setFavoritesCount (fc + if fv then -1 else 1)
            setFavorited (not fv)
            launchAff_ do
              if fv then do
                r <- unfavorite cu'.token slug
                liftEffect $ setFavoritesCount r.article.favoritesCount
              else do
                r <- favorite cu'.token slug
                liftEffect $ for_ r (_.article.favoritesCount >>> setFavoritesCount)
      ]
  let favoriteText = nut (text (isFavorited <#> if _ then "Favorited" else "Favorite Post"))
  let img = pure (D.Src := image)
  let authorName = nut (text_ username)
  let fCount = nut (text (show <$> favoritesCount))
  article_ ~~
    { title: nut (D.h1_ [ text_ title ])
    , image1: img
    , image2: img
    , image3: img
    , image4: img
    , image5: img
    , body: nut (D.p_ [ text_ body ])
    , description: nut (D.p_ [ text_ description ])
    , author1: authorName
    , author2: authorName
    , author3: authorName
    , author4: authorName
    , favoriteAttrs1: favoriteAttrs
    , favoriteAttrs2: favoriteAttrs
    , favoriteText1: favoriteText
    , favoriteText2: favoriteText
    , followAttrs1: followAttrs
    , followAttrs2: followAttrs
    , followText1: followText
    , followText2: followText
    , articleHeader: nut (D.h2_ [ text_ title ])
    , favoritesCount1: fCount
    , favoritesCount2: fCount
    }
