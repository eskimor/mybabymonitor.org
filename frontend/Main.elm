module Main where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal
import Mouse
import List
import Time
import Date
import Signal ((<~), Signal)
import Maybe (withDefault, Maybe)

import Server
import Baby

type NavItem = Baby | Parent

type alias Action = State -> State
    
type alias State = {
      selectedNavItem : NavItem
    , babiesOnline : Int
    , availableBabies : List String
    , currentDate : Date.Date
    , serverError : String
    }


-- Actions:
selectNavItem : NavItem -> Action
selectNavItem item model = { model | selectedNavItem <- item }

updateDate : Time.Time -> Action
updateDate time model = { model | currentDate <- Date.fromTime time }
---
step : Action -> State -> State
step = identity

-- View:
view : State -> Html
view model =
    div [ id "pageContainer"]
        [
         navbar model
        , div [ id "pageContent" ]
              [
               Baby.view (Baby.initialState Baby.emptyStorage)
              ]
        , footer [class "navbar navbar-default navbar-fixed-bottom footer"]
                 [
                  babiesOnlineText model
                  , br [] []
                  , small [] [copyrightNotice model]]
        ]


navbar : State -> Html
navbar model =
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
                       (List.map (navMenuItem model) [Baby, Parent])
                   ]

             ]
         ]

navMenuItem : State -> NavItem -> Html
navMenuItem model item =
    let active = item == model.selectedNavItem
    in
      li [ classList [ ("active", active) ] ]
         [ a
           [
            href "javascript:;"
           , onClick (Signal.send updates (selectNavItem item))
           ]
           [
            text << toString <| item
           , srOnly (if active then "current" else "")
           ]
         ]


babiesOnlineText : State -> Html
babiesOnlineText model = text <| case model.babiesOnline of
                           0 -> "All babies are awake and with their parents."
                           1 -> "One baby online."
                           n -> toString n ++ " babies online."
                           
main : Signal Html
main = Signal.map view model

model : Signal State
model = Signal.foldp step initialModel inputSignal

inputSignal : Signal Action
inputSignal = Signal.mergeMany [
               (fromServer <~ Server.connect)
              , (Signal.subscribe updates)
              , (updateDate <~ Time.every Time.hour)
              ]

fromServer : Result String Server.Incoming -> Action
fromServer rmessage model = case rmessage of
                              Err message ->
                                  { model | serverError <- message }
                              Ok message ->
                                  case message of
                                       Server.BabyCount num ->
                                           { model | babiesOnline <- num }
                                       Server.AvailableBabies babies ->
                                           { model | availableBabies <- babies }
                                          

               
updates : Signal.Channel Action
updates = Signal.channel identity


type alias JSState = {
      selectedNavItem : String
    , babiesOnline : Int
    , currentDate : Float
    }
    
port initialJSModel : JSState 

initialModel : State
initialModel =
    { selectedNavItem = case initialJSModel.selectedNavItem of
                                             "Baby" -> Baby
                                             "Parent" -> Parent
    , babiesOnline = initialJSModel.babiesOnline
    , availableBabies = []
    , currentDate = Date.fromTime(initialJSModel.currentDate)
    , serverError = ""
    }

copyrightNotice : State -> Html
copyrightNotice model = let currentYear = Date.year model.currentDate
                            startYear = 2015
                            yearSpan = if currentYear - startYear > 0
                                       then toString startYear ++ " - " ++ toString currentYear
                                       else toString startYear
                        in text <| "copyright by Robert Klotzner " ++ yearSpan

-- Screen reader only:
srOnly : String -> Html
srOnly t = span [ class "sr-only" ][ text t ]

type alias Storage = {
      baby : Baby.Storage
    }

storage : Storage 
storage = withDefault { baby = Baby.emptyStorage } getStorage


updateStorage : Signal.Channel Storage
updateStorage = Signal.channel { baby = Baby.emptyStorage }
          
port getStorage : Maybe Storage
     
port setStorage : Signal Storage
port setStorage = Signal.subscribe updateStorage
     