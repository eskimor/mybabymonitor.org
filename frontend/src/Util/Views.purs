module Util.Views where
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B

-- Screen reader only:
srOnly :: forall p m . String -> H.HTML p m
srOnly t = H.span [ A.class_ B.srOnly ] [ H.text t ]
