module Handler.Home where

import Import

import qualified Handler.Session as S

import Handler.Common
import Handler.Baby
import Handler.Parent
import BabyPhone.BabyCommunication

getHomeR :: Handler Html
getHomeR = do
  babies <- retrieveBabies
  babyName <- S.lookupSession S.BabyName
  intro:baby:parent:[] <- sequence . take 3 . repeat $ newIdent
  
  ((_, babyWidget), babyEncType) <- generateFormGet $ babyForm babyName
  ((_, parentWidget), parentEncType) <- generateFormGet $ parentForm babies
  defaultLayout $ layout Home $(widgetFile "home")

    
    



  
