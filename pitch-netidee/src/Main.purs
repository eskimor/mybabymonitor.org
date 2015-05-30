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
import Types
import Slide1
import Slide2
import Slide3
import Slide4
import Slide5
import Slide6
  
init :: State
init = Slide1             

-- Actions:
nextSlide :: Action
nextSlide Slide1 = Slide2
nextSlide Slide2 = Slide3
nextSlide Slide3 = Slide4
nextSlide Slide4 = Slide5
nextSlide Slide5 = Slide6


prevSlide :: Action
prevSlide Slide1 = Slide1
prevSlide Slide2 = Slide1
prevSlide Slide3 = Slide2
prevSlide Slide4 = Slide3
prevSlide Slide5 = Slide4
prevSlide Slide6 = Slide5

--

view :: forall p m . (Applicative m) => State -> Slide p m
view Slide1 = masterLayout slide1
view Slide2 = slideLayout slide2
view Slide3 = slideLayout slide3 
view Slide4 = slideLayout slide4 
view Slide5 = slideLayout slide5 
view Slide6 = slideLayout slide6 

masterLayout :: forall p m . (Applicative m) => Slide p m -> Slide p m 
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

slideLayout :: forall p m . (Applicative m) => Slide p m -> Slide p m
slideLayout content = masterLayout $
     H.div [ A.class_ contents ]
      [
        content
      , H.div
         [ A.class_ slideLogo ]
         [ H.img [ A.src "pix/logo.svg" ][] ]
      ]

handleMasterClick ::  forall m . (Applicative m) => A.Event A.MouseEvent -> A.EventHandler (m Action)
handleMasterClick ev = case ev.button of
  0 -> pure (pure nextSlide) -- Primary button
  2 -> pure (pure prevSlide) 
  1 -> pure (pure prevSlide) -- Not working
  _ -> pure (pure id)

handleMasterKeyPress :: A.Event A.KeyboardEvent -> Action
handleMasterKeyPress ev = case ev.keyCode of
  37 -> prevSlide -- arrow left
  39 -> nextSlide -- arrow right 
  32 -> nextSlide -- space
  _ -> id
  
  

ui :: forall p m . (Applicative m) => Component p m Action Action
ui = component (view <$> stateful init update)
  where
    update = flip ($)

main = do
     Tuple node driver <- runUI ui
     appendToBody node




--

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node
