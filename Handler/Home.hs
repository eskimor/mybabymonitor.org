module Handler.Home where

import Import

import qualified Handler.Session as S
import Text.Julius (rawJS)

import Handler.Common
import Handler.Baby
import Handler.Parent
import Yesod.WebSockets
import BabyMonitor.BabyCommunication


getHomeR :: Handler Html
getHomeR = do
  babies <- retrieveBabies
  babyName <- S.lookupSession S.BabyName
  let navItem = "Grouping" :: Text -- Fixme - be more intelligent
  let babiesOnline = "2" :: Text
  let isGrouped = True
  webSockets clientSocket
  intro:baby:parent:parentBaby:[] <- sequence . take 4 . repeat $ newIdent
  
  ((_, babyWidget), babyEncType) <- generateFormGet $ babyForm babyName
  ((_, parentWidget), parentEncType) <- generateFormGet $ parentForm babies
  defaultLayout $ do
             $(widgetFile "home")
             $(widgetFile "home-js")

    
    
clientSocket :: WebSocketsT Handler ()
clientSocket = return ()
  


  
