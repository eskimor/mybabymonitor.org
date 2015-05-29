module Main where
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

data State = Slide1 | Slide2 | Slide3

init :: State
init = Slide1             
       
-- Actions:

type Action = State -> State

nextSlide :: Action
nextSlide Slide1 = Slide2
nextSlide Slide2 = Slide3
nextSlide Slide3 = Slide3


prevSlide :: Action
prevSlide Slide1 = Slide1
prevSlide Slide2 = Slide1
prevSlide Slide3 = Slide2



view :: forall p m . (Applicative m) => State -> H.HTML p (m Action)
view Slide1 = masterLayout $
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

view Slide2 = slideLayout $ 
              H.img [ A.src "pix/babyslidesalad.svg", A.type_ "image/svg+xml"
                       ] []

view _ = masterLayout $ H.text "Nothing here!"


masterLayout :: forall p m . (Applicative m) => H.HTML p (m Action) -> H.HTML p (m Action)
masterLayout content = 
  H.div
   [ A.classes [neutralBg, feetBg]
   , tabIndex 1
   , A.onmouseup handleMasterClick
   , A.onkeypress (A.input handleMasterKeyPress)
   ]
   [
     content
   ]

handleMasterClick ::  forall m . (Applicative m) => A.Event A.MouseEvent -> A.EventHandler (m Action)
handleMasterClick ev = case ev.button of
  0 -> pure (pure nextSlide) -- Primary button
  2 -> do
    A.preventDefault
    A.stopImmediatePropagation
    pure (pure prevSlide) 
  1 -> pure (pure prevSlide) -- Not working
  _ -> pure (pure id)

handleMasterKeyPress :: A.Event A.KeyboardEvent -> Action
handleMasterKeyPress ev = case ev.keyCode of
  37 -> prevSlide
  39 -> nextSlide 
  _ -> id
  
  
slideLayout :: forall p m . (Applicative m) => H.HTML p (m Action) -> H.HTML p (m Action)
slideLayout content = masterLayout $
     H.div [ A.class_ contents ]
      [
        content
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



backgroundHeading heading arg text =
  H.div (arg ++ [ A.class_ background ])
   [ 
     heading [] [H.text text ]
   ]

-- Until my pull request gets merged:
data_ :: forall i. String -> A.Attr i
data_ = A.attr $ A.attributeName "data"

tabIndex :: forall i. Number -> A.Attr i
tabIndex = A.attr (A.attributeName "tabIndex") <<< show

onContextMenu :: forall i. (A.Event A.MouseEvent -> A.EventHandler i) -> A.Attr i
onContextMenu = A.handler (A.eventName "oncontextmenu")
--

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node
