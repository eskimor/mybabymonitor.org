module Util.NavigationBar where
    
import Util.Views (..)
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal
import List

-- Model:
      
type alias Model = {
      navigationPoints : List String
    , selected : String
    }

init : String -> List String -> Model
init selected navigationPoints = { navigationPoints = navigationPoints
                                 , selected = selected }

updates : Signal.Channel Action
updates = Signal.channel identity

-- Actions:
type alias Action = Model -> Model

selectNavigation : String -> Action
selectNavigation nav model = { model | selected <- nav }


-- View : 

view : Model -> Html
view model =
    nav [ class "navbar navbar-default"] 
        [
         div [ class "container-fluid" ]
             [
              div [ class "navbar-header" ]
                  [
                   a [ class "navbar-brand", href "#" ][ text "mybabymonitor.org" ]
                   , button [ type' "button", class "navbar-toggle collapsed"
                          , attribute "data-toggle" "collapse"
                          , attribute "data-target" "#main-navbar"]
                          [
                           srOnly "Toggle navigation"
                          , span [ class "icon-bar" ][]
                          , span [ class "icon-bar" ][]
                          , span [ class "icon-bar" ][]
                          ]
                  ]
             , div [ class "collapse navbar-collapse", id "main-navbar"]
                   [ 
                    ul [ class "nav navbar-nav"]
                       <| viewNavigationPoints model
                   ]

             ]
         ]

viewNavigationPoints : Model -> List Html
viewNavigationPoints model = List.map (viewNavigationPoint model) model.navigationPoints
                             
viewNavigationPoint : Model -> String -> Html
viewNavigationPoint model item =
    let active = item == model.selected
    in
      li [ classList [ ("active", active) ] ]
         [ a
           [
            href "javascript:;"
           , onClick (Signal.send updates (selectNavigation item))
           ]
           [
            text item
           , srOnly (if active then "current" else "")
           ]
         ]
