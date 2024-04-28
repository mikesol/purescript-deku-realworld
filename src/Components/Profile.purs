module Components.Profile where

import Prelude

import API.Types (Article, AuthState(..), MultipleArticles, SingleProfile)
import Components.Favorited (doFavoriting)
import FRP.Poll (Poll)
import Components.Following (followAttrs, followText)
import Data.Foldable (oneOf)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.Control (text, text_)
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.DOM.Combinators (runOn_)
import Deku.DOM.Attributes as DA
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.DOM.Listeners as DL
import Deku.Pursx (pursx)

data ProfileStatus = ProfileLoading | ProfileLoaded SingleProfile MultipleArticles MultipleArticles

type SingleArticle =
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
  pursx @SingleArticle
    { author1: authorHref
    , author2: authorHref
    , image: authorImg
    , signedOutButton
    , signedInButton
    , favoritesCount1: fc
    , favoritesCount2: fc
    , name: fixed [ text_ username ]
    , date: fixed [ text_ (prettyDate updatedAt) ]
    , title: fixed [ D.h1_ [ text_ title ] ]
    , description: fixed [ D.p_ [ text_ description ] ]
    , toArticle
    }
  where
  authorHref = DA.href_ ("/#/profile/" <> username)
  authorImg = DA.src_ image
  toArticle = DA.href_ ("/#/article/" <> slug)

type Profile =
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

type ProfileLoading =
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

profile :: Poll AuthState -> Poll ProfileStatus -> Nut
profile e pstat = pstat <#~> case _ of
  ProfileLoaded a b c -> profileLoaded e a b c
  ProfileLoading -> pursx @ProfileLoading {}

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
  pursx @Profile
    { image1: oneOf [ DA.src_ image ]
    , name1: fixed [ D.h4_ [ text_ username ] ]
    , bio1: fixed [ maybe mempty (\b -> D.h4_ [ text_ b ]) bio ]
    , name2: fixed [ text_ username ]
    , followAttrs: oneOf followAttrs'
    , followText: followText'
    , favoritedAttributes:
        oneOf
          [ DA.klass $ tab <#> \ct -> "nav-link" <> case ct of
              FavoritedArticles -> " active"
              MyArticles -> ""
          , DA.style_ "cursor: pointer;"
          , runOn_ DL.click (setTab FavoritedArticles)
          ]
    , myAttributes:
        oneOf
          [ DA.klass $ tab <#> \ct -> "nav-link" <> case ct of
              FavoritedArticles -> ""
              MyArticles -> " active"
          , DA.style_ "cursor: pointer;"
          , runOn_ DL.click (setTab MyArticles)
          ]
    , articleList:
        let
          su = singleArticle currentUser
        in
          D.div_
            [ tab <#~> case _ of
                FavoritedArticles -> D.div_ (map su favoritedArticles.articles)
                MyArticles -> D.div_ (map su myArticles.articles)
            ]
    }
