module Slide2 where
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
import Types
    
slide2 :: forall p m . (Applicative m) => Slide p m 
slide2 = H.img [ A.src "pix/babyslidesalad.svg", A.type_ "image/svg+xml"] []

