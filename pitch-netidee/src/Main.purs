module Main where
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as A
import qualified Halogen.HTML.Events.Types as A
import qualified Halogen.Themes.Bootstrap3 as B
import Data.Tuple(Tuple(..))
import Halogen
import Halogen.Component
import Halogen.Signal
import Control.Monad.Eff
import qualified Data.List as L
import Data.List(List(..), fromArray)
import DOM
import MyClasses
import Types
import Slide1
import Slide2
import Slide3
import Slide4
import Slide5
import Slide6
import Slide8

init :: forall p m . (Applicative m) => State p m
init = State (fromArray [slide1, slide2, slide3, slide4, slide5, slide6, slide8]) Nil

-- Actions:
nextSlide :: forall p m . Action p m
nextSlide o@(State (Cons _ Nil) b) = o
nextSlide (State o@(Cons p r) b) = State r (Cons o b)


prevSlide :: forall p m . Action p m
prevSlide o@(State _ Nil) = o
prevSlide (State _ (Cons p bs)) = State p bs
--

view :: forall p m . (Applicative m) => State p m -> Slide p m
view (State (Cons s ss) Nil) = masterLayout s
view (State (Cons s ss) _) = slideLayout s

masterLayout :: forall p m . (Applicative m) => Slide p m -> Slide p m
masterLayout content =
    H.div
    [ A.classes [neutralBg, feetBg, container]
    , tabIndex 1
    , A.onmouseup handleMasterClick
    , A.onkeypress (A.input handleMasterKeyPress)
    , A.attr (A.attributeName "autofocus") "true"
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
         [ A.classes [slideLogo] ]
         [ H.img [ A.src "pix/logo.svg" ][] ]
      ]

handleMasterClick ::  forall p m . (Applicative m) => A.Event A.MouseEvent -> A.EventHandler (m (Action p m))
handleMasterClick ev = case ev.button of
  0 -> pure (pure nextSlide) -- Primary button
  2 -> pure (pure prevSlide)
  1 -> pure (pure prevSlide) -- Not working
  _ -> pure (pure id)

handleMasterKeyPress :: forall p m . A.Event A.KeyboardEvent -> Action p m
handleMasterKeyPress ev = case ev.keyCode of
  37 -> prevSlide -- arrow left
  39 -> nextSlide -- arrow right
  0 -> nextSlide -- space
  _ -> id



ui :: forall p m . (Applicative m) => Component p m (Action p m) (Action p m)
ui = component (view <$> stateful init update)
  where
    update = flip ($)

main :: Eff (halogen :: Halogen.HalogenEffects, dom :: DOM) Unit
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
