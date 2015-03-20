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
import Util.NavigationBar as NavBar

-- Model:

type alias Action = Model -> Model
    
type alias Model = {
    navBar : NavBar.Model
    , babiesOnline : Int
    , availableBabies : List String
    , currentDate : Date.Date
    , serverError : String
    , isGrouped : Bool
    }


init : Model
init =
    { navBar = NavBar.init
                    serverInput.selectedNavigation
                    [ "Baby","Parent","Grouping" ]
    , babiesOnline = serverInput.babiesOnline
    , availableBabies = []
    , currentDate = Date.fromTime(serverInput.currentDate)
    , serverError = ""
    , isGrouped = serverInput.isGrouped
    }

model : Signal Model
model = Signal.foldp step init inputSignal

inputSignal : Signal Action
inputSignal = Signal.mergeMany [
               (fromServer <~ Server.connect)
              , (updateNavBar <~ Signal.subscribe NavBar.updates)
              , (updateDate <~ Time.every Time.hour)
              ]



-- Actions:

updateDate : Time.Time -> Action
updateDate time model = { model | currentDate <- Date.fromTime time }

updateNavBar : NavBar.Action -> Action
updateNavBar nact model = { model | navBar <- nact model.navBar }
                          
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
                                           
---
step : Action -> Model -> Model
step = identity

-- View:
view : Model -> Html
view model =
    div [ id "pageContainer"]
        [
         NavBar.view model.navBar
        , div [ id "pageContent" ]
              [
               viewContent model
              ]
        , footer [class "navbar navbar-default navbar-fixed-bottom footer"]
                 [
                  viewBabiesOnlineText model
                  , br [] []
                  , small [] [viewCopyrightNotice model]]
        ]


viewContent : Model -> Html
viewContent model = if (model.navBar.selected == "Baby"
                        || model.navBar.selected == "Parent")
                       && not model.isGrouped
                    then
                        div [ class "alert alert-info"
                            , attribute "role" "alert" ]
                            [
                             text "In order to ensure that babies can only be watched or heard by their parents, this device needs to be grouped first. Please create or join a group: "
                            , button [ onClick (Signal.send
                                                 NavBar.updates
                                                 (NavBar.selectNavigation "Grouping"))
                                ]
                                [ text "Grouping" ]
                            ]
                    else
                        case model.navBar.selected of
                          "Baby" -> Baby.view (Baby.initialModel Baby.emptyStorage)
                          _ -> text " Sorry - nothing here yet!"
                             
                        
viewBabiesOnlineText : Model -> Html
viewBabiesOnlineText model = text <| case model.babiesOnline of
                           0 -> "All babies are awake and with their parents."
                           1 -> "One baby online."
                           n -> toString n ++ " babies online."

viewCopyrightNotice : Model -> Html
viewCopyrightNotice model = let currentYear = Date.year model.currentDate
                                startYear = 2015
                                yearSpan = if currentYear - startYear > 0
                                           then toString startYear ++ " - " ++ toString currentYear
                                           else toString startYear
                            in text <| "copyright by Robert Klotzner " ++ yearSpan

-- Input / output:

main : Signal Html
main = Signal.map view model

type alias ServerInput = {
      selectedNavigation : String
    , babiesOnline : Int
    , currentDate : Float
    , isGrouped : Bool
    }
    
port serverInput : ServerInput



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
     