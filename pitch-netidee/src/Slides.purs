module Slides where
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

slide1 :: forall p m . (Monad m) => Slide p m
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

    
slide2 :: forall p m . (Monad m) => Slide p m 
slide2 = H.img [ A.src "pix/babyslidesalad.svg", A.type_ "image/svg+xml"] []

slide3 :: forall p m . (Monad m) => Slide p m
slide3 =
  H.div [ A.class_ container ]
   [
     slide2
   , H.div [ A.id_ "no-really-all", A.class_ container ]
     [
       H.div [ A.id_ "no-really-inner" , A.class_ container] 
        [
          H.img [ A.src "pix/no_really.svg", A.type_ "image/svg+xml"] []
        ]
     ]
   ]

slide4 :: forall p m . (Monad m) => Slide p m
slide4 = H.img [ A.id_ "telecommpix", A.src "pix/telecommunications.svg", A.type_ "image/svg+xml"] []


slide5 :: forall p m . (Monad m) => Slide p m
slide5 = H.img [ A.src "pix/technicalOverview.svg", A.type_ "image/svg+xml"] []

slide6 :: forall p m . (Monad m) => Slide p m
slide6 = H.img [ A.src "pix/technicalOverviewAnimated.svg", A.type_ "image/svg+xml"] []


slide8 :: forall p m . (Monad m) => Slide p m
slide8 = H.img [ A.src "pix/androidapps.svg", A.type_ "image/svg+xml"] []
