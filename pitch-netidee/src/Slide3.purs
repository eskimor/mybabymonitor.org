module Slide3 where
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
import Slide2

slide3 :: forall p m . (Applicative m) => Slide p m
slide3 =
  H.div [A.style (A.styles (fromList [(Tuple "height" "100%")]))]
   [
     slide2
   , H.div [ A.id_ "no-really-all" ]
     [
       H.div [ A.id_ "no-really-inner" ] 
        [
          H.img [ A.src "pix/no_really.svg", A.type_ "image/svg+xml"] []
        ]
     ]
   ]
