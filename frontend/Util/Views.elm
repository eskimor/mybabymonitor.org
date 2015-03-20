module Util.Views where
import Html (..)
import Html.Attributes (..)

-- Screen reader only:
srOnly : String -> Html
srOnly t = span [ class "sr-only" ][ text t ]