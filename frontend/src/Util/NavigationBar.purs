module Util.NavigationBar where

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.Themes.Bootstrap3.InputGroup as BI

import qualified  Data.Array as Array


import qualified Util.Views as UV

type State = {
      navigationPoints :: [String]
    , selected :: String
    }

init :: String -> [String] -> State
init selected navigationPoints = { navigationPoints : navigationPoints
                                 , selected : selected }

-- Actions:
type Action = State -> State

selectNavigation :: String -> Action
selectNavigation nav state = state { selected = nav }



view :: forall p m . (Applicative m) => State -> H.HTML p (m Action)
view state =
    H.nav [ A.classes [B.navbar, B.navbarDefault] ]
     [
       H.div [ A.class_ B.containerFluid ]
        [
          H.div [ A.class_ B.navbarHeader ]
           [
             H.a [ A.class_ B.navbarBrand, A.href "#" ]
              [ H.text "mybabymonitor.org" ]
           , H.button [ A.type_ "button", A.classes [B.navbarToggle, B.collapse]
                      , A.attr (A.attributeName "data-toggle") "collapse"
                      , A.attr (A.attributeName "data-target") "#main-navbar" ]
              [
                UV.srOnly "Toggle navigation"
              , H.span [ A.class_ B.iconBar ] []
              , H.span [ A.class_ B.iconBar ] []
              , H.span [ A.class_ B.iconBar ] []
              ]
           ]
        , H.div [ A.classes [B.collapse, B.navbarCollapse], A.id_ "main-navbar" ]
           [
             H.ul [ A.classes [B.nav, B.navbarNav] ] $ viewNavigationPoints state
           ]
        ]
     ]


    
viewNavigationPoints :: forall p m . (Applicative m) => State -> [H.HTML p (m Action)]
viewNavigationPoints state = viewNavigationPoint state <$> state.navigationPoints
                             
viewNavigationPoint :: forall p m . (Applicative m) => State -> String -> H.HTML p (m Action)
viewNavigationPoint state item =
    let active = item == state.selected
    in
      H.li (if active then [ A.class_ B.active ] else [])
       [ H.a
          [
            A.href "javascript:;"
          , A.onclick (A.input \_ -> selectNavigation item)
          ]
          [
            H.text item
          , UV.srOnly (if active then "current" else "")
          ]
       ]
