module Components.Article where

import Prelude

import API.Effects (addComment, deleteComment, favorite, follow, unfavorite, unfollow)
import API.Types (AuthState(..), Comment, SingleArticle, isSignedIn, whenSignedIn)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (for_, oneOf, oneOfMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.Attribute ((:=))
import Deku.Control (dyn_, text, text_)
import Deku.Core (class Korok, Domable, bus, insert_, remove)
import Deku.DOM as D
import Deku.Do (useMemoized, useState, useState')
import Deku.Do as Deku
import Deku.Listeners (click)
import Deku.Pursx (nut, (~~))
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import FRP.Dedup (dedup)
import FRP.Event (AnEvent, keepLatest)
import Type.Proxy (Proxy(..))
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLTextAreaElement (fromElement, value)
import Web.HTML.Window (document)

data ArticleStatus = ArticleLoading | ArticleLoaded SingleArticle (Array Comment)

data CommentText = CommentText String | NoText

derive instance Eq CommentText


articleLoading_ =
  Proxy :: Proxy
         """<div class="article-page">

    <div class="banner">
        <div class="container"><h1>Loading...</h1></div></div></div>

"""

myComment_ =
  Proxy :: Proxy
         """
                <div class="card">
                    <div class="card-block">
                        <p class="card-text">~body~</p>
                    </div>
                    <div class="card-footer">
                        <a ~profile1~ class="comment-author">
                            <img ~imgsrc~ class="comment-author-img"/>
                        </a>
                        &nbsp;
                        <a ~profile2~ class="comment-author">~username~</a>
                        <span class="date-posted">~date~</span>
                        <span class="mod-options">
              <!-- <i class="ion-edit"></i> -->
              <i ~deleteAction~ class="ion-trash-a"></i>
            </span>
                    </div>
                </div>"""

theirComment_ =
  Proxy :: Proxy
         """<div class="card">
                    <div class="card-block">
                        <p class="card-text">~body~</p>
                    </div>
                    <div class="card-footer">
                        <a ~profile1~ class="comment-author">
                            <img ~imgsrc~ class="comment-author-img"/>
                        </a>
                        &nbsp;
                        <a ~profile2~ class="comment-author">~username~</a>
                        <span class="date-posted">~date~</span>
                    </div>
                </div>"""

article_ =
  Proxy :: Proxy
         """<div class="article-page">

    <div class="banner">
        <div class="container">

            ~title~

            <div class="article-meta">
                <a ~authProf1~><img ~image1~ /></a>
                <div class="info">
                    <a ~authProf2~ class="author">~author1~</a>
                    <span class="date">~lastUpdated~</span>
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
                <a ~authProf3~><img ~image2~ /></a>
                <div class="info">
                    <a ~authProf4~ class="author">~author3~</a>
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

                <div class="card comment-form">
                    <div class="card-block">
                        <textarea ~commentTextCommand~ class="form-control" placeholder="Write a comment..." rows="3"></textarea>
                    </div>
                    <div class="card-footer">
                        <img ~image3~ class="comment-author-img"/>
                        <button ~commentButtonCommand~ class="btn btn-sm btn-primary">
                            Post Comment
                        </button>
                    </div>
                </div>

                <!-- comments here -->
                ~commentList~

            </div>

        </div>

    </div>

</div>
"""

article :: forall s m lock payload. Korok s m => AnEvent m AuthState -> ArticleStatus -> Domable m lock payload
article e (ArticleLoaded a cmt) = articleLoaded e a cmt
article e ArticleLoading = articleLoading_ ~~ {}

articleLoaded :: forall s m lock payload. Korok s m => AnEvent m AuthState -> SingleArticle -> Array Comment -> Domable m lock payload
articleLoaded
  currentUser
  { article:
      { title
      , slug
      , updatedAt
      , favoritesCount: favC
      , description
      , body
      , favorited
      , author:
          { username
          , image
          , following
          }
      }
  }
  comments = Deku.do
  setFollowing /\ isFollowing <- useState following
  setFavorited /\ isFavorited <- useState favorited
  setNewComment /\ newComment <- useState'
  setFavoritesCount /\ favoritesCount <- useMemoized ((_ <|> pure favC) >>> dedup)
  let
    followAttrs = oneOf
      [ currentUser <#> \cu -> D.Class := ("btn btn-sm btn-outline-secondary" <> if isSignedIn cu then "" else " disabled")
      , click $ ({ cu: _, flw: _ } <$> currentUser <*> isFollowing) <#> \{ cu, flw } -> do
          whenSignedIn cu \cu' -> do
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
      [ currentUser <#> \cu -> D.Class := ("btn btn-sm btn-outline-primary" <> if isSignedIn cu then "" else " disabled")
      , click $ ({ cu: _, fv: _, fc: _ } <$> currentUser <*> isFavorited <*> favoritesCount) <#> \{ cu, fv, fc } -> do
          whenSignedIn cu \cu' -> do
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
  let authProf = pure (D.Href := "/#/profile/" <> username)
  let authorName = nut (text_ username)
  let fCount = nut (text (show <$> favoritesCount))
  article_ ~~
    { title: nut (D.h1_ [ text_ title ])
    , image1: img
    , image2: img
    , image3: img
    , authProf1: authProf
    , authProf2: authProf
    , authProf3: authProf
    , authProf4: authProf
    , body: nut (D.p_ [ text_ body ])
    , description: nut (D.p_ [ text_ description ])
    , author1: authorName
    , author2: authorName
    , author3: authorName
    , author4: authorName
    , commentTextCommand: oneOf
        [ pure $ D.Id := "comment-text"
        ]
    , commentButtonCommand: oneOf
        [ click $ currentUser <#> \cu -> do
              elt <- window >>= document >>= getElementById "comment-text" <<< toNonElementParentNode <<< toDocument
              for_ (elt >>= fromElement) \ta -> do
                v <- value ta
                whenSignedIn cu \cu' -> do
                    launchAff_ $ do
                        addComment cu'.token slug v >>= case _ of
                            Right c -> liftEffect $ setNewComment c.comment
                            Left e -> throwError (error (show e))
        ]
    , lastUpdated: nut (text_ (prettyDate updatedAt))
    , favoriteAttrs1: favoriteAttrs
    , favoriteAttrs2: favoriteAttrs
    , favoriteText1: favoriteText
    , favoriteText2: favoriteText
    , followAttrs1: followAttrs
    , followAttrs2: followAttrs
    , followText1: followText
    , followText2: followText
    , articleHeader: nut $ D.h2_ [ text_ title ]
    , favoritesCount1: fCount
    , favoritesCount2: fCount
    , commentList: nut $ dyn_ D.div
        ( ({ cu: _, com: _ } <$> currentUser <*> (newComment <|> oneOfMap pure comments)) <#> \{ cu, com } -> keepLatest $ bus \setDelete delete -> do
            let body = nut (text_ com.body)
            let username = nut (text_ com.author.username)
            let imgsrc = pure (D.Src := com.author.image)
            let profile = pure (D.Href := "/#/profile/" <> com.author.username)
            let profile1 = profile
            let profile2 = profile
            let date = nut (text_ (prettyDate com.updatedAt))
            (delete $> remove) <|>
              ( pure
                  $ insert_
                  $ maybe (theirComment_ ~~ { body, imgsrc, profile1, profile2, username, date })
                      ( \u -> do
                          let
                            deleteAction = click $ pure do
                              launchAff_ $ deleteComment u.token slug com.id
                              setDelete unit

                          myComment_ ~~ { body, imgsrc, profile1, profile2, username, date, deleteAction }
                      )
                      ( case cu of
                          SignedIn u
                            | u.username == com.author.username -> Just u
                            | otherwise -> Nothing
                          SignedOut -> Nothing
                      )
              )
        )
    }
