module Slide5 where
import Data.Tuple
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as A
import qualified Halogen.HTML.Events.Types as A
import qualified Halogen.Themes.Bootstrap3 as B
import Data.StrMap
import Halogen
import Halogen.Component
import Halogen.Signal
import Control.Monad.Eff
import DOM
import MyClasses
import Types

slide5 :: forall p m . (Applicative m) => Slide p m
slide5 = H.img [ A.src "pix/androidapps.svg", A.type_ "image/svg+xml"] []
