module Components.Profile where

import Prelude

import API.Types (Article, AuthState(..), MultipleArticles, SingleProfile)
import Components.Favorited (doFavoriting)
import Components.Following (followAttrs, followText)
import Data.Foldable (oneOf)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.Attribute ((:=))
import Deku.Control (blank, switcher_, text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Deku
import Deku.Listeners (click)
import Deku.Pursx (nut, (~~))
import FRP.Event (Event)
import Type.Proxy (Proxy(..))

data ProfileStatus = ProfileLoading | ProfileLoaded SingleProfile MultipleArticles MultipleArticles

singleArticle_ =
  Proxy
    :: Proxy
         """<div class="article-preview">
    <div class="article-meta">
        <a ~author1~><img ~image~ /></a>
        <div class="info">
            <a ~author2~ class="author">~name~</a>
            <span class="date">~date~</span>
        </div>
        <div ~signedOutButton~>
            <i class="ion-heart"></i> ~favoritesCount1~
        </div>
        <button ~signedInButton~>
            <i class="ion-heart"></i> ~favoritesCount2~
        </button>
    </div>
    <a ~toArticle~ class="preview-link">
        ~title~
        ~description~
        <span>Read more...</span>
        <!-- <ul class="tag-list">
            <li class="tag-default tag-pill tag-outline">Music</li>
            <li class="tag-default tag-pill tag-outline">Song</li>
        </ul> -->
    </a>
</div>"""

singleArticle :: forall lock payload. Event AuthState -> Article -> Domable lock payload
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
  let fc = nut (text (show <$> favoritesCount))
  let
    signedOutButton = oneOf
      [ pure $ D.Class := "text-success btn-sm pull-xs-right"
      , currentUser <#> \cu -> D.Style := case cu of
          SignedIn _ -> "display:none;"
          SignedOut -> ""
      ]
  let
    signedInButton = oneOf
      [ pure $ D.Class := "btn btn-outline-primary btn-sm pull-xs-right"
      , currentUser <#> \cu -> D.Style := case cu of
          SignedIn _ -> ""
          SignedOut -> "display:none;"
      , doFavoriting currentUser slug isFavorited favoritesCount setFavoritesCount setFavorited
      ]
  singleArticle_ ~~
    { author1: authorHref
    , author2: authorHref
    , image: authorImg
    , signedOutButton
    , signedInButton
    , favoritesCount1: fc
    , favoritesCount2: fc
    , name: nut (text_ username)
    , date: nut (text_ (prettyDate updatedAt))
    , title: nut (D.h1_ [ text_ title ])
    , description: nut (D.p_ [ text_ description ])
    , toArticle
    }
  where
  authorHref = pure (D.Href := "/#/profile/" <> username)
  authorImg = pure (D.Src := image)
  toArticle = pure (D.Href := "/#/article/" <> slug)

profile_ =
  Proxy
    :: Proxy
         """<div class="profile-page">

    <div class="user-info">
        <div class="container">
            <div class="row">

                <div class="col-xs-12 col-md-10 offset-md-1">
                    <img ~image1~ class="user-img"/>
                    ~name1~
                    <p>
                        ~bio1~
                    </p>
                    <button ~followAttrs~ class="btn btn-sm btn-outline-secondary action-btn">
                        <i class="ion-plus-round"></i>
                        &nbsp;
                        ~followText~ ~name2~
                    </button>
                </div>

            </div>
        </div>
    </div>

    <div class="container">
        <div class="row">

            <div class="col-xs-12 col-md-10 offset-md-1">
                <div class="articles-toggle">
                    <ul class="nav nav-pills outline-active">
                        <li class="nav-item">
                            <a ~myAttributes~ class="nav-link active">My Articles</a>
                        </li>
                        <li class="nav-item">
                            <a ~favoritedAttributes~ class="nav-link">Favorited Articles</a>
                        </li>
                    </ul>
                </div>
                ~articleList~
            </div>

        </div>
    </div>

</div>
"""

profileLoading_ =
  Proxy
    :: Proxy
         """<div class="profile-page">

    <div class="user-info">
        <div class="container">
            <div class="row">

                <div class="col-xs-12 col-md-10 offset-md-1">
                    <h4>Loading...</h4>
                    </div>
                    </div>
                    </div>
                    </div>
                    </div>
"""

profile :: forall lock payload. Event AuthState -> ProfileStatus -> Domable lock payload
profile e (ProfileLoaded a b c) = profileLoaded e a b c
profile _ ProfileLoading = profileLoading_ ~~ {}

data Tab = MyArticles | FavoritedArticles

profileLoaded :: forall lock payload. Event AuthState -> SingleProfile -> MultipleArticles -> MultipleArticles -> Domable lock payload
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
  profile_ ~~
    { image1: pure (D.Src := image)
    , name1: nut (D.h4_ [ text_ username ])
    , bio1: nut (maybe blank (\b -> D.h4_ [ text_ b ]) bio)
    , name2: nut (text_ username)
    , followAttrs: followAttrs'
    , followText: followText'
    , favoritedAttributes: oneOf
        [ tab <#> \ct -> D.Class := "nav-link" <> case ct of
            FavoritedArticles -> " active"
            MyArticles -> ""
        , pure $ D.Style := "cursor: pointer;"
        , click $ pure $ setTab FavoritedArticles
        ]
    , myAttributes: oneOf
        [ tab <#> \ct -> D.Class := "nav-link" <> case ct of
            FavoritedArticles -> ""
            MyArticles -> " active"
        , pure $ D.Style := "cursor: pointer;"
        , click $ pure $ setTab MyArticles
        ]
    , articleList:
        let
          su = singleArticle currentUser
        in
          nut $ tab # switcher_ D.div case _ of
            FavoritedArticles -> D.div_ (map su favoritedArticles.articles)
            MyArticles -> D.div_ (map su myArticles.articles)
    }
