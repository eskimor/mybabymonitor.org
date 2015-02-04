module Handler.Home where

import Import
import Yesod.WebSockets
import Network.Socket (SockAddr(..), PortNumber(..))

import BabyPhone.BabyCommunication
import qualified Handler.Session as S
import Handler.Common


getHomeR :: Handler Html
getHomeR = do
  babies <- retrieveBabies
  babyName <- S.lookupSession S.BabyName
  intro:baby:parent:[] <- sequence . take 3 . repeat $ newIdent
  
  ((_, babyWidget), babyEncType) <- generateFormGet $ babyForm babyName
  ((_, parentWidget), parentEncType) <- generateFormGet $ parentForm babies
  defaultLayout $ do
            setTitle "mybabymonitor.org - Web based baby monitor"
            $(widgetFile "home")

             
getBabyR :: Handler Html
getBabyR = do
  ((result, _), _) <- runFormGet $ babyForm (Just "baby")
  let babyName = case result of
                    FormSuccess (Just n) -> n
                    _ -> "baby"
  S.setSession S.BabyName babyName
  defaultLayout $ $(widgetFile "baby") >> babyParentCommon

getParentR :: Handler Html
getParentR = do
  ((result, _), _) <- retrieveBabies >>= runFormGet . parentForm 
  let babyName = case result of
                   FormSuccess n -> n
                   _ -> "baby"
  defaultLayout $ $(widgetFile "parent") >> babyParentCommon

babyParentCommon :: Widget
babyParentCommon = $(widgetFile "common") >> jqueryLib

getBabiesR :: Handler TypedContent
getBabiesR = do retrieveBabies >>= selectRep . provideRep . returnJson


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


connectBaby :: BabyName -> WebSocketsT Handler ()
connectBaby name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  $logDebug $ "connect Baby got address: " <> tshow addr
  mBaby <- liftIO . atomically $ do
    connections <- readTVar $ babyConnections y
    let (mBaby, connections') = popBabyConnection connections addr name
    writeTVar (babyConnections y) connections'
    return mBaby
  $logDebug "After tvar!"
  case mBaby of
    Nothing -> sendTextData $ "{\"error\" : \"You don't have a baby named " <> name <> "\"}"
    Just connection ->
        let
            writer :: WebSocketsT Handler ()
            writer = receiveData >>= liftIO . atomically . parentSend connection
            reader = (liftIO . atomically . parentReceive) connection >>= sendTextData
        in
         do
          $logDebug "We found your baby, stay put!"
          liftIO . atomically . parentSend connection $ "{\"startStreaming\" : true}"
          race_
             (catch (forever writer) printException)
             (catch (forever reader) printException)
    where
      printException :: SomeException -> WebSocketsT Handler ()
      printException e = $logDebug $ "Parent end communication exited with exception: " <> tshow e

    
    
getBabyOpenChannelR :: BabyName -> Handler ()
getBabyOpenChannelR = webSockets . babyWaiting 

getBabyConnectChannelR :: BabyName -> Handler ()
getBabyConnectChannelR = webSockets . connectBaby

parentForm :: [BabyName] -> Html -> MForm Handler (FormResult BabyName, Widget)
parentForm babies = renderDivs $ areq (selectFieldList (zip babies babies)) "Connect to child: " baby
    where
      baby = case babies of
        b:_ -> Just b
        []  -> Nothing


babyForm :: (Maybe BabyName) ->  Html -> MForm Handler (FormResult (Maybe BabyName), Widget)
babyForm n = renderDivs $ aopt textField "Start baby monitor for: " (Just n)

retrieveBabies :: Handler [BabyName]
retrieveBabies = do
  App {babyConnections = connections} <- getYesod
  address <- getClientAddress
  liftIO . atomically $ fmap fst . flip getBabies address <$> readTVar connections


getClientAddress :: Handler SockAddr
--getClientAddress = filterPort . remoteHost <$> waiRequest
getClientAddress = return $ SockAddrUnix ""


filterPort :: SockAddr -> SockAddr
filterPort (SockAddrInet _ address) = SockAddrInet (PortNum 0) address
filterPort (SockAddrInet6 _ flow address scope) = SockAddrInet6 (PortNum 0) flow address scope
filterPort s = s

