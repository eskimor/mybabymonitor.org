module Slide1 where
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

slide1 :: forall p m . (Applicative m) => Slide p m
slide1 =
     H.div [ A.class_ contents ]
      [
        H.div
         [ A.class_ titleLogo ]
         [ H.img [ A.src "pix/logo.svg" ][] ]
      , H.br [] []
      , backgroundHeading H.h1 [] "MyBabyMonitor.org"
      , H.br [] []
      , backgroundHeading H.h2 [A.id_ "secondTitleHeading"] "web-based baby monitor"
      ]


backgroundHeading heading arg text =
  H.div (arg ++ [ A.class_ background ])
   [ 
     heading [] [H.text text ]
   ]
