module Main where
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as A
import qualified Halogen.HTML.Events.Types as A
import qualified Halogen.Themes.Bootstrap3 as B
import Halogen.Internal.VirtualDOM(Widget(..))
import Data.Tuple(Tuple(..))
import Halogen
import Halogen.Component
import Halogen.Signal
import Control.Monad.Eff
import Halogen.HTML.Events.Monad(Event(..))
import qualified Data.List as L
import Data.List(List(..), fromArray)
import DOM
import MyClasses
import Types
import Slides

slides :: forall p m . (Monad m) => [Slide p m]
slides = (<$>) slideLayout [every4Seconds, aBaby, felix, babyMonitors, babyMonitorsCrossed]
         <> [masterLayout intro]
         <> (<$>) slideLayout [browserWebRTC, security, sustainabilityMostConvenient, sustainabilityFree, sustainabilityBest, sustainabilityHardware, sustainability1, sustainability2, sustainability3, androidApps, duringVideo]

init :: forall p m . (Monad m) => State p m
init = State (fromArray slides) Nil

-- Actions:

update :: forall p m . State p m -> Action -> State p m
update p NextSlide = nextSlide p
update p PrevSlide = prevSlide p
update p DoNothing = p

nextSlide :: forall p m . State p m -> State p m
nextSlide o@(State (Cons _ Nil) b) = o
nextSlide (State o@(Cons p r) b) = State r (Cons o b)


prevSlide :: forall p m . State p m -> State p m
prevSlide o@(State _ Nil) = o
prevSlide (State _ (Cons p bs)) = State p bs
--

view :: forall p m . (Monad m) => State p m -> Slide p m
view (State (Cons s ss) _) = s

masterLayout :: forall p m . (Monad m) => Slide p m -> Slide p m
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

slideLayout :: forall p m . (Monad m) => Slide p m -> Slide p m
slideLayout content = masterLayout $
     H.div [ A.class_ contents ]
      [
        content
      , H.div
         [ A.classes [slideLogo] ]
         [ H.img [ A.src "pix/logo.svg" ][] ]
      ]

handleMasterClick ::  forall p m . (Monad m) => A.Event A.MouseEvent -> A.EventHandler (m Action)
handleMasterClick ev = case ev.button of
  0 -> pure (pure NextSlide) -- Primary button
  2 -> pure (pure PrevSlide)
  1 -> pure (pure PrevSlide) -- Not working
  _ -> pure (pure DoNothing)

handleMasterKeyPress :: forall p m . A.Event A.KeyboardEvent -> Action
handleMasterKeyPress ev = case ev.keyCode of
  37 -> PrevSlide -- arrow left
  39 -> NextSlide -- arrow right
  0 -> NextSlide -- space
  _ -> DoNothing




ui :: forall p m . (Monad m) => Component p m Action Action
ui = component (view <$> stateful init update)

--main :: Eff (halogen :: Halogen.HalogenEffects, dom :: DOM) Unit
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
