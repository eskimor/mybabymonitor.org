module Types where
import Data.Tuple
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as A
import qualified Halogen.HTML.Events.Types as A
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Data.List as L
import Data.List(List(..)) 
import Halogen
import Halogen.Component
import Halogen.Signal
import Control.Monad.Eff
import DOM
import MyClasses

data State p m = State (List (Slide p m)) (List (List (Slide p m)))

       
-- Actions:

type Action p m = State p m -> State p m

type Slide p m = H.HTML p (m (Action p m))

-- Until my pull request gets merged:
data_ :: forall i. String -> A.Attr i
data_ = A.attr $ A.attributeName "data"

tabIndex :: forall i. Number -> A.Attr i
tabIndex = A.attr (A.attributeName "tabIndex") <<< show

onContextMenu :: forall i. (A.Event A.MouseEvent -> A.EventHandler i) -> A.Attr i
onContextMenu = A.handler (A.eventName "oncontextmenu")
