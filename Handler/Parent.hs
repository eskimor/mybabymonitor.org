module Handler.Parent where

import Import
import Yesod.WebSockets
import Network.Socket (SockAddr(..))

import BabyPhone.BabyCommunication
import qualified Handler.Session as S
import Handler.Common


getParentR :: Handler Html
getParentR = do
  eventLog <- newIdent
  ((result, _), _) <- retrieveBabies >>= runFormGet . parentForm 
  let babyName = case result of
                   FormSuccess n -> n
                   _ -> "baby"
  defaultLayout $ layout Parent $ $(widgetFile "parent") >> babyParentCommon eventLog

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

getBabyConnectChannelR :: BabyName -> Handler ()
getBabyConnectChannelR = webSockets . connectBaby


parentForm :: [BabyName] -> Html -> MForm Handler (FormResult BabyName, Widget)
parentForm babies = renderDivs $ areq (selectFieldList (zip babies babies)) "Connect to child: " baby
    where
      baby = case babies of
        b:_ -> Just b
        []  -> Nothing

