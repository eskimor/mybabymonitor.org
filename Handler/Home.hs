module Handler.Home where

import Import

import qualified Handler.Session as S
-- import Text.Julius (rawJS)

import Yesod.WebSockets
    
import qualified BabyMonitor.Server as Server


getHomeR :: Handler Html
getHomeR = do
  mdeviceId <-  S.lookupSession S.DeviceId
  deviceId <- case mdeviceId of
                Nothing -> do
                  uid <- liftIO $ Server.makeDeviceId
                  S.setSession S.DeviceId uid
                  return uid
                Just uid -> return uid
  mfamilyId <- S.lookupSession S.FamilyId
  webSockets $ clientSocket deviceId mfamilyId
  defaultLayout $ do
             [whamlet|<h1> To be defined |]
             addScript $ StaticR js_App_js
             -- $(widgetFile "home")
             -- $(widgetFile "home-js")



clientSocket :: Server.DeviceId -> Maybe Server.FamilyId -> WebSocketsT Handler ()
clientSocket did mfid = do
  conn <- ask
  serv <- lift $ server <$> getYesod
  client <- liftIO . Server.makeClient conn did mfid $ serv
  forever $ do
    msg <- receiveData
    server <$> getYesod >>= liftIO . Server.handleMessage client mfid msg
    

putMakeFamilyR :: Handler Text
putMakeFamilyR = do
  mfid <- S.lookupSession S.FamilyId :: Handler (Maybe Server.FamilyId)
  case mfid of
    Just _ -> return "Please leave your current family first."
    Nothing -> do
              fid <- liftIO Server.makeFamilyId
              S.setSession S.FamilyId fid
              return "Ok."

putJoinFamilyR :: Handler Text
putJoinFamilyR = do
  mdid <- S.lookupSession S.DeviceId
  case mdid of
    Nothing -> return "Please visit '/' first and get invited to a family!"
    Just did -> do
                   mfid <- getYesod >>= liftIO . Server.acceptInvitation did . server 
                   case mfid of
                     Nothing -> return "Please get invited to a family first!"
                     Just fid -> S.setSession S.FamilyId fid >> return "Ok."
   

putDeclineInvitationR :: Handler Text
putDeclineInvitationR = do
  S.deleteSession S.DeviceId -- We delete the client id for security
                             -- reasons.  If the user declines an
                             -- invitation, it is likely that someone
                             -- guessed his id, so give him a new one.
  return "Ok."

putLeaveFamilyR :: Handler Text
putLeaveFamilyR = do
  S.deleteSession S.FamilyId
  return "Ok."


  
