module Types where
import Data.Tuple
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as A
import qualified Halogen.HTML.Events.Types as A
import qualified Halogen.Themes.Bootstrap3 as B
import Halogen
import Halogen.Component
import Halogen.Signal
import Control.Monad.Eff
import DOM
import MyClasses

data State = Slide1 | Slide2 | Slide3 | Slide4 | Slide5 | Slide6

       
-- Actions:

type Action = State -> State

type Slide p m = H.HTML p (m Action)

-- Until my pull request gets merged:
data_ :: forall i. String -> A.Attr i
data_ = A.attr $ A.attributeName "data"

tabIndex :: forall i. Number -> A.Attr i
tabIndex = A.attr (A.attributeName "tabIndex") <<< show

onContextMenu :: forall i. (A.Event A.MouseEvent -> A.EventHandler i) -> A.Attr i
onContextMenu = A.handler (A.eventName "oncontextmenu")
