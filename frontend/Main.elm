module Main where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal
import Mouse
import List


type NavItem = Baby | Parent

type alias Action = State -> State
    
type alias State = {
      selectedNavItem : NavItem
    }


-- Actions:
selectNavItem : NavItem -> Action
selectNavItem item model = { model | selectedNavItem <- item }

---
step : Action -> State -> State
step = identity

view : State -> Html
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
                       (List.map (viewNavMenuItem model) [Baby, Parent])
                   ]

             ]
         ]

viewNavMenuItem : State -> NavItem -> Html
viewNavMenuItem model item =
    let active = item == model.selectedNavItem
    in
      li [ classList [ ("active", active) ] ]
         [ a [ href "javascript:;", onClick (Signal.send updates (selectNavItem item)) ][ text << toString <| item, srOnly (if active then "current" else "") ]]

-- Screen reader only:
srOnly : String -> Html
srOnly t = span [ class "sr-only" ][ text t ]
                                              
                           
main : Signal Html
main = Signal.map view model


model : Signal State
model = Signal.foldp step {selectedNavItem=Baby} <| Signal.subscribe updates

updates : Signal.Channel Action
updates = Signal.channel identity