module Handler.Baby where

import Import

import Yesod.WebSockets

import BabyPhone.BabyCommunication
import qualified Handler.Session as S
import Handler.Common

getBabyR :: Handler Html
getBabyR = do
  eventLog <- newIdent
  ((result, _), _) <- runFormGet $ babyForm (Just "baby")
  let babyName = case result of
                    FormSuccess (Just n) -> n
                    _ -> "baby"
  S.setSession S.BabyName babyName
  defaultLayout $ layout Baby $ $(widgetFile "baby") >> babyParentCommon eventLog

                
babyWaiting :: BabyName -> WebSocketsT Handler ()
babyWaiting name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  $logDebug $ "waiting Baby got address: " <> tshow addr
  connection <- liftIO . atomically $ do
    connections <-  readTVar $ babyConnections y
    (connection, connections') <- newBaby connections addr name
    writeTVar (babyConnections y) connections'
    return connection
  let writer = receiveData >>= liftIO . atomically . babySend connection 
  let reader = (liftIO . atomically . babyReceive) connection >>= sendTextData
  let updateConnections' = updateConnections y addr connection
  race_
                   (catch (forever writer) updateConnections')
                   (catch (forever reader) updateConnections')
  where
    updateConnections :: App -> SockAddr ->  BabyConnection -> SomeException -> WebSocketsT Handler ()
    updateConnections y addr connection e = do
                     $logDebug $ "Baby end communication exited with exception: " <> tshow e
                     liftIO . atomically $ modifyTVar' (babyConnections y)
                          (\bcs -> dropBabyConnection bcs addr (name, connection))

                          
getBabyOpenChannelR :: BabyName -> Handler ()
getBabyOpenChannelR = webSockets . babyWaiting 

babyForm :: (Maybe BabyName) ->  Html -> MForm Handler (FormResult (Maybe BabyName), Widget)
babyForm n = renderDivs $ aopt textField "Start baby monitor for: " (Just n)
