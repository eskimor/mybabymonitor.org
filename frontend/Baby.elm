module Baby where

import Html (..)
import Html.Attributes (..)
import Signal



type alias State = {
      connection : ConnectionState
    , babyName : String
    , userGuide : List String
    }

type ConnectionState = Stopped
                     | Started
                     | Authorized
                     | WaitingForParent
                     | Connected
                     | Streaming
                     | Error Error

            
type Error = AuthorizationError | NoServerConnection 

initialState : Storage -> State
initialState storage = { connection = Stopped    
               , babyName = storage.babyName
               , userGuide = []
               }


type alias Storage = {
      babyName : String
    }
    
emptyStorage : Storage
emptyStorage = {
      babyName = ""
    }

view : State -> Html
view _ =
    div []
        [
         div [class "input-group", style [("width", "100%")]]
             [
              input [type' "text", id "babiesName"
                    , class "form-control"
                    , attribute "placeholder" "Baby's name (optional)"
                    ][]
             , span [ class "input-group-btn", id "startMonitorButtonSpan" ]
                    [
                     button [ type' "button", class "btn btn-default "
                             , id "startMonitorButton"
                             ] [ text "Start baby monitor" ]
                    ]
             ]
         , div [][]
        ]