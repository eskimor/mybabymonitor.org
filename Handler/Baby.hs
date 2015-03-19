module Handler.Baby where

import Import

import Yesod.WebSockets

import BabyMonitor.BabyCommunication
import qualified Handler.Session as S
import Handler.Common
import Control.Exception.Lifted (onException)

getBabyR :: Handler Html
getBabyR = do
  eventLog <- newIdent
  ((result, _), _) <- runFormGet $ babyForm (Just "baby")
  let babyName = case result of
                    FormSuccess (Just n) -> n
                    _ -> "baby"
  S.setSession S.BabyName babyName
  defaultLayout $  $(widgetFile "baby") >> babyParentCommon eventLog

                
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
                   (onException (forever writer) updateConnections')
                   (onException (forever reader) updateConnections')
  where
    updateConnections :: App -> SockAddr ->  BabyConnection -> WebSocketsT Handler ()
    updateConnections y addr connection = do
                     liftIO . atomically $ modifyTVar' (babyConnections y)
                          (\bcs -> dropBabyConnection bcs addr (name, connection))

                          
getBabyOpenChannelR :: BabyName -> Handler ()
getBabyOpenChannelR = webSockets . babyWaiting 

babyForm :: (Maybe BabyName) ->  Html -> MForm Handler (FormResult (Maybe BabyName), Widget)
babyForm n = renderDivs $ aopt textField "Start baby monitor for: " (Just n)
