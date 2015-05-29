module Main where
import Data.Tuple
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.Themes.Bootstrap3 as B
import Halogen
import Halogen.Component
import Halogen.Signal
import Control.Monad.Eff
import DOM
import MyClasses

data State = Slide1 | Slide2 | Slide3


init :: State
init = Slide2             
       
-- Actions:

type Action = State -> State

nextSlide :: Action
nextSlide Slide1 = Slide2
nextSlide Slide2 = Slide3


prevSlide :: Action
prevSlide Slide1 = Slide1
prevSlide Slide2 = Slide1
prevSlide Slide3 = Slide2



view :: forall p m . (Applicative m) => State -> H.HTML p (m Action)
view Slide1 =
     H.div
      [ A.classes [neutralBg, feetBg, contents] ]
      [
        H.div
         [ A.class_ titleLogo ]
         [ H.img [ A.src "pix/logo.svg" ][] ]
      , H.br [] []
      , backgroundHeading H.h1 [] "MyBabyMonitor.org"
      , H.br [] []
      , backgroundHeading H.h2 [A.id_ "secondTitleHeading"] "web-based baby monitor"
      ]

view Slide2 = viewWithBg $ 
              H.object [ data_ "pix/babyslidesalad.svg", A.type_ "image/svg+xml"
                       ] []
  

backgroundHeading heading arg text =
  H.div (arg ++ [ A.class_ background ])
   [ 
     heading [] [H.text text ]
   ]


viewWithBg :: forall p m . (Applicative m) => H.HTML p (m Action) -> H.HTML p (m Action)
viewWithBg content =
     H.div
      [ A.classes [neutralBg, feetBg] ]
      [
        H.div [ A.class_ contents ]
        [
          content
        ]
      , H.div
         [ A.class_ slideLogo ]
         [ H.img [ A.src "pix/logo.svg" ][] ]
      ]

ui :: forall p m . (Applicative m) => Component p m Action Action
ui = component (view <$> stateful init update)
  where
    update = flip ($)

main = do
     Tuple node driver <- runUI ui
     appendToBody node

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

-- Until my pull request gets merged:
data_ :: forall i. String -> A.Attr i
data_ = A.attr $ A.attributeName "data"
