module Components.Create where


import API.Types (User)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (blank)
import Deku.Core (class Korok, Domable, Nut)
import Deku.Do (useState)
import Deku.Do as Deku
import Deku.Pursx (nut, (~~))
import FRP.Event (AnEvent)
import Type.Proxy (Proxy(..))

create_ =
  Proxy :: Proxy """<div class="editor-page">
    <div class="container page">
        <div class="row">

            <div class="col-md-10 offset-md-1 col-xs-12">
                <div>
                    <fieldset>
                        ~titleField~
                        <!--<fieldset class="form-group">
                            <input type="text" class="form-control form-control-lg" placeholder="Article Title" />
                        </fieldset>-->
                        ~descriptionField~
                        <!-- <fieldset class="form-group">
                            <input type="text" class="form-control" placeholder="What's this article about?" />
                        </fieldset> -->
                        ~bodyField~
                        <!--<fieldset class="form-group">
                            <textarea class="form-control" rows="8"
                                      placeholder="Write your article (in markdown)"></textarea>
                        </fieldset>-->
                        ~tagsField~
                        <!--<fieldset class="form-group">
                            <input type="text" class="form-control" placeholder="Enter tags" />
                            <div class="tag-list"></div>
                        </fieldset>-->
                        <button class="btn btn-lg pull-xs-right btn-primary" type="button">
                            Publish Article
                        </button>
                    </fieldset>
                </div>
            </div>

        </div>
    </div>
</div>
"""

create :: forall s m lock payload. Korok s m => AnEvent m User -> Domable m lock payload
create user = Deku.do
  setErrors /\ errors <- useState []
  setTitle /\ title <- useState Nothing
  setDescription /\ description <- useState Nothing
  setBody /\ body <- useState Nothing
  setTags /\ tags <- useState Nothing


  create_ ~~ {
    titleField: nut blank,
    descriptionField: nut blank,
    bodyField: nut blank,
    tagsField: nut blank
   }