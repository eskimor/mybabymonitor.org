module Main where

import Html (..)
import Html.Attributes (..)
import Signal
import Mouse


type SelectedWidget = Baby | Parent
    
type alias State = {
      selectedTab : SelectedWidget
    }
    

view : State -> Html
view model =
    nav [ class "navbar navbar-default"] 
        [
         div [ class "container-fluid" ]
             [
              div [ class "navbar-header" ]
                  [
                   button [ type' "button", class "navbar-toggle collapsed"
                          , attribute "data-toggle" "collapse"
                          , attribute "data-target" "#main-navbar"]
                          [
                           srOnly "Toggle navigation"
                          , span [ class "icon-bar" ][]
                          , span [ class "icon-bar" ][]
                          , span [ class "icon-bar" ][]
                          ]
                  , a [ class "navbar-brand", href "#" ][ text "mybabymonitor.org" ]
                  ]
             , div [ class "collapse navbar-collapse", id "main-navbar"]
                   [ 
                    ul [ class "nav navbar-nav"]
                       [
                        li [ class "active" ]
                           [ a [ href "#" ][ text "Baby", srOnly "current"]]
                       , li [][ a [ href "#"][ text "Parent" ]]
                       ]
                   ]
             ]
         ]
-- Screen reader only:
srOnly : String -> Html
srOnly t = span [ class "sr-only" ][ text t ]
                                              
                           
main : Signal Html
main = Signal.map view model


model : Signal State
model = Signal.foldp (\_ _ -> {selectedTab=Baby}) ({selectedTab=Baby}) Mouse.clicks
