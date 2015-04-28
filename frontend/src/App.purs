module App where

import qualified Data.Map as Map
import qualified Data.Date as Date
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.Themes.Bootstrap3 as B
import Data.Bifunctor


import qualified Util.NavigationBar as NavBar

type State = {
    navBar :: NavBar.State
    , inFamily :: Boolean
    , babiesOnline :: Map.Map String String
    , babiesCount :: Number
    , serverError :: String
    }

init :: State
init =
  {
    navBar : NavBar.init "Family" ["Baby", "Parent", "Family"]
  , babiesOnline : Map.empty
  , babiesCount : 0
  , serverError : ""
  , inFamily : false
  }


-- Actions:
type Action = State -> State

updateNavBar :: NavBar.Action -> Action
updateNavBar act state = state { navBar = act state.navBar }

navbarView :: forall p m . (Applicative m) => H.HTML p (m NavBar.Action) -> H.HTML p (m Action)
navbarView = rmap (updateNavBar <$>)

view :: forall p m . (Applicative m) => State -> H.HTML p (m Action)
view state =
    H.div [ A.id_ "pageContainer"]
     [
      navbarView (NavBar.view state.navBar)
     , H.div [ A.id_ "pageContent" ]
        [
         viewContent state
        ]
     , H.footer
        [A.classes [B.navbar, B.navbarDefault, B.navbarFixedBottom, A.className "footer"] ]
        [
         viewBabiesOnlineText state
        , H.br [] []
        , H.small [] [viewCopyrightNotice state]
        ]
      ]
 
-- Views:

type View = forall p m . (Applicative m) => State -> H.HTML p (m Action)
    
viewContent :: View
viewContent state = if (state.navBar.selected == "Baby"
                        || state.navBar.selected == "Parent")
                       && not state.inFamily
                    then
                        H.div [ A.classes [B.alert, B.alertInfo]
                              , A.attr (A.attributeName "role") "alert" ]
                         [
                          H.text "In order to ensure that babies can only be watched or heard by their parents, this device needs to be part of a family first. Please create or join a family: "
                         , H.button [ A.onclick (A.input \_ -> 
                                                 updateNavBar (NavBar.selectNavigation "Family"))
                                    ]
                            [ H.text "Grouping" ]
                         ]
                    else
                        case state.navBar.selected of
                          _ -> H.text " Sorry - nothing here yet!"
                             
                        
viewBabiesOnlineText :: View
viewBabiesOnlineText state = H.text case state.babiesCount of
                           0 -> "All babies are awake and with their parents."
                           1 -> "One baby online."
                           n -> show n ++ " babies online."

viewCopyrightNotice :: View
viewCopyrightNotice state = let currentYear = 2018 -- FIXME
                                startYear = 2015
                                yearSpan = if currentYear - startYear > 0
                                           then show startYear ++ " - " ++ show currentYear
                                           else show startYear
                            in H.text $ "copyright by Robert Klotzner " ++ yearSpan
