module Slide6 where
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

slide6 :: forall p m . (Applicative m) => Slide p m
slide6 =
  H.div [A.style (A.styles (fromList [(Tuple "height" "100%")]))]
   [
     H.img [ A.src "pix/yeswecanlogos.svg", A.type_ "image/svg+xml"] []
   , H.div [ A.class_ flexColumn, A.id_ "no-really-all" ]
      [
        H.div [ A.classes [centeredText] ]
         [
           H.div [ A.class_ background ]
            [
              H.h1 [] [H.text "Yes We Can!"]
            ]
         ]
      ]
   ]