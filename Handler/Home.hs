module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Yesod.WebSockets
import qualified Network.WebSockets as WS
import Network.Socket (SockAddr(..), PortNumber(..), HostAddress(..))
import Text.Julius

import BabyPhone.BabyCommunication
import qualified Handler.Session as S

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  babies <- retrieveBabies
  babyName <- S.lookupSession S.BabyName
  ((_, babyWidget), babyEncType) <- generateFormGet $ babyForm babyName
  ((_, parentWidget), parentEncType) <- generateFormGet $ parentForm babies
  defaultLayout $ do
            setTitle "mybabymonitor.org - Web based baby monitor"
            $(widgetFile "home")

             
getBabyR :: Handler Html
getBabyR = do
  ((result, _), _) <- runFormGet $ babyForm (Just "baby")
  let babyName = case result of
                    FormSuccess n -> n
                    _ -> "baby"
  S.setSession S.BabyName babyName
  defaultLayout $ $(widgetFile "baby") >> babyParentCommon

getParentR :: Handler Html
getParentR = do
  ((result, parentWidget), parentEncType) <- retrieveBabies >>= runFormGet . parentForm 
  let babyName = case result of
                   FormSuccess n -> n
                   _ -> "baby"
  defaultLayout $ $(widgetFile "parent") >> babyParentCommon

babyParentCommon :: Widget
babyParentCommon = $(widgetFile "common") 
                   >> addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"

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

babyForm :: (Maybe BabyName) ->  Html -> MForm Handler (FormResult BabyName, Widget)
babyForm n = renderDivs $ areq textField "Start baby monitor for: " n

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

