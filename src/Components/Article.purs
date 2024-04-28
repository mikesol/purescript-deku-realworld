module Components.Article where

import Prelude

import API.Effects (addComment, deleteComment)
import API.Types (AuthState(..), Comment, SingleArticle, isSignedIn, whenSignedIn)
import Components.Favorited (doFavoriting)
import Components.Following (followAttrs, followText)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (oneOf, oneOfMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\))
import Date (prettyDate)
import Deku.Control (text, text_)
import Deku.Core (Nut, fixed, useDynAtBeginning)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Combinators (injectElementT, runOn, runOn_)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useHot, useState, useState', (<#~>))
import Deku.Pursx (pursx)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import FRP.Poll (Poll)
import Record (union)
import Web.HTML.HTMLTextAreaElement (value)

data ArticleStatus = ArticleLoading | ArticleLoaded SingleArticle (Array Comment)

data CommentText = CommentText String | NoText

derive instance Eq CommentText

type ArticleLoading =
  """<div class="article-page"> 
    <div class="banner">
        <div class="container"><h1>Loading...</h1></div>
    </div>
</div>
"""

type MyComment =
  """ <div class="card">
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
</div>
"""

type TheirComment =
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
</div>
"""

type Article =
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

                <div ~postComment~ class="card comment-form">
                    <div class="card-block">
                        ~commentTextArea~
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

article :: Poll AuthState -> Poll ArticleStatus -> Nut
article e astat = astat <#~> case _ of
  ArticleLoaded a cmt -> articleLoaded e a cmt
  ArticleLoading -> pursx @ArticleLoading {}

articleLoaded :: Poll AuthState -> SingleArticle -> Array Comment -> Nut
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
  setCommentTA /\ commentTA <- useState'
  setFavoritesCount /\ favoritesCount <- useHot favC
  let followAttrs' = followAttrs username currentUser isFollowing setFollowing
  let followText' = followText isFollowing
  let
    favoriteAttrs = oneOf
      [ DA.klass_ "btn btn-sm btn-outline-primary"
      , DA.style $ currentUser <#> \cu -> if isSignedIn cu then "" else "display: none;"
      , doFavoriting currentUser slug isFavorited favoritesCount setFavoritesCount setFavorited
      ]
  let favoriteText = fixed [ text (isFavorited <#> if _ then "Favorited" else "Favorite Post") ]
  let img = DA.src_ image
  let
    myImg = DA.src $ currentUser <#>
      ( fromMaybe "https://picsum.photos/200"
          <<< case _ of
            SignedIn u -> u.image
            SignedOut -> Nothing
      )
  let authProf = DA.href_ $ "/#/profile/" <> username
  let authorName = fixed [ text_ username ]
  let fCount = fixed [ text (show <$> favoritesCount) ]
  pursx @Article
    { title: fixed [ D.h1_ [ text_ title ] ]
    , image1: img
    , image2: img
    , image3: myImg
    , authProf1: authProf
    , authProf2: authProf
    , authProf3: authProf
    , authProf4: authProf
    , body: fixed [ D.p_ [ text_ body ] ]
    , description: fixed [ D.p_ [ text_ description ] ]
    , author1: authorName
    , author2: authorName
    , author3: authorName
    , author4: authorName
    , commentTextArea: fixed
        [ D.textarea
            [ injectElementT setCommentTA
            , DA.klass_ "form-control"
            , DA.placeholder_ "Write a comment..."
            , DA.rows_ "3"
            ]
            []
        ]
    , commentButtonCommand: oneOf
        [ runOn DL.click $ ({ cu: _, ta: _ } <$> currentUser <*> commentTA) <#> \{ cu, ta } -> do
            v <- value ta
            whenSignedIn cu \cu' -> do
              launchAff_ $ do
                addComment cu'.token slug v >>= case _ of
                  Right c -> liftEffect $ setNewComment c.comment
                  Left e -> throwError (error (show e))
        ]
    , lastUpdated: fixed [ text_ (prettyDate updatedAt) ]
    , favoriteAttrs1: favoriteAttrs
    , favoriteAttrs2: favoriteAttrs
    , favoriteText1: favoriteText
    , favoriteText2: favoriteText
    , followAttrs1: oneOf followAttrs'
    , followAttrs2: oneOf followAttrs'
    , followText1: followText'
    , followText2: followText'
    , postComment: oneOf [ DA.style $ currentUser <#> \cu -> if isSignedIn cu then "" else "display: none;" ]
    , articleHeader: fixed [ D.h2_ [ text_ title ] ]
    , favoritesCount1: fCount
    , favoritesCount2: fCount
    , commentList: D.div_
        [ Deku.do
            { value: { cu, com }, remove } <- useDynAtBeginning (({ cu: _, com: _ }) <$> currentUser <*> (newComment <|> oneOfMap pure comments))
            let profile = DA.href_ $ "/#/profile/" <> com.author.username
            let
              common =
                { body: fixed [ text_ com.body ]
                , imgsrc: DA.src_ com.author.image
                , profile1: profile
                , profile2: profile
                , username: fixed [ text_ com.author.username ]
                , date: fixed [ text_ (prettyDate com.updatedAt) ]
                }
            maybe (pursx @TheirComment common)
              ( \u -> do
                  let
                    deleteAction = runOn_ DL.click do
                      launchAff_ $ deleteComment u.token slug com.id
                      remove
                  pursx @MyComment (common `union` { deleteAction })
              )
              ( case cu of
                  SignedIn u
                    | u.username == com.author.username -> Just u
                    | otherwise -> Nothing
                  SignedOut -> Nothing

              )
        ]
    }
