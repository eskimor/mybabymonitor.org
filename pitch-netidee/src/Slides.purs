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

intro :: forall p m . (Monad m) => Slide p m
intro =
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

    
babyMonitors :: forall p m . (Monad m) => Slide p m 
babyMonitors = H.img [ A.src "pix/babyslidesalad.svg", A.type_ "image/svg+xml"] []

babyMonitorsCrossed :: forall p m . (Monad m) => Slide p m
babyMonitorsCrossed =
  H.div [ A.class_ container ]
   [
     babyMonitors
   , H.div [ A.id_ "no-really-all", A.class_ container ]
     [
       H.div [ A.id_ "no-really-inner" , A.class_ container] 
        [
          H.img [ A.src "pix/no_really.svg", A.type_ "image/svg+xml"] []
        ]
     ]
   ]

felix :: forall p m . (Monad m) => Slide p m
felix = H.div [ A.id_ "felixOuter"]
         [
           H.div [ A.id_ "felixInner"]
            [
              H.img [ A.id_ "felixImg", A.src "pix/felix.jpg", A.type_ "image/jpg"] []
            ]
         ]

every4Seconds :: forall p m . (Monad m) => Slide p m
every4Seconds =
  H.div [ A.id_ "every4SecondsOuter" ]
   [
     H.div [ A.id_ "every4SecondsInner" ]
      [
        H.div [] [ H.h4 [] [ H.text "Every"]]
      , H.div [] [ H.h4 [] [ H.text "4"]]
      , H.div [] [ H.h4 [] [ H.text "Seconds"]]
      ]
   ]

aBaby :: forall p m . (Monad m) => Slide p m
aBaby =
  H.div [ A.id_ "every4SecondsOuter" ]
   [
     H.div [ A.id_ "every4SecondsInner" ]
      [
        H.div [] [ H.h4 [] [ H.text "A"]]
      , H.div [] [ H.h4 [] [ H.text "Baby"]]
      , H.div [] [ H.h4 [] [ H.text "is Born"]]
      ]
   ]

browserWebRTC :: forall p m . (Monad m) => Slide p m 
browserWebRTC =
  H.div [ A.id_ "felixOuter"]
   [
     H.div [ A.id_ "no-really-inner" ]
      [
        H.img [ A.id_ "browserWebRTC", A.src "pix/browserWebRTC.svg", A.type_ "image/svg+xml"] []
      ]
   ]

security :: forall p m . (Monad m) => Slide p m
security =
  H.div [ A.id_ "every4SecondsOuter" ]
   [
     H.div [ A.id_ "every4SecondsInner" ]
      [
        H.div [] [ H.h4 [] [ H.text "Secure Invitation System"]]
      , H.div [] [ H.h4 [] [ H.text "Encrypted"]]
      , H.div [] [ H.h4 [] [ H.text "Peer-To-Peer"]]
      ]
   ]
