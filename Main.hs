{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Main where


import Yesod
import Text.Julius
import Text.Hamlet
import qualified Data.Text as T
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit as C
import Data.Conduit (($$), (=$))
import Control.Applicative ((<$>),(<*>))
import Network.Socket (SockAddr(..), PortNumber(..), HostAddress(..))
import Yesod.WebSockets
import qualified Network.WebSockets as WS
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Aeson
import Control.Exception.Base (SomeException)
import Control.Exception.Lifted (catch)
import Network.Wai (remoteHost)
import Control.Monad 
import Data.Monoid ((<>))
import Yesod.Core.Json
import Control.Monad.Reader (ask)
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)

import BabyCommunication




mkYesod "BabyPhone" [parseRoutes|
                      / HomeR GET
                      /baby BabyR GET
                      /parent ParentR GET
                      /babies BabiesR GET
                      /babyOpenChannel/#T.Text BabyOpenChannelR GET -- Opened by the baby
                      /babyConnectChannel/#T.Text BabyConnectChannelR GET -- Connected by the parent
|]

instance Yesod BabyPhone where
    approot = ApprootStatic "http://localhost:3000"
    shouldLog _ _ _ = True -- good for development
    makeSessionBackend _ = Just <$> defaultClientSessionBackend minutes "client_session_key.aes"
                           where minutes = 5256000 -- 10 years ~ forever 

instance RenderMessage BabyPhone FormMessage where
    renderMessage _ _ = defaultFormMessage


data BabyPhone = BabyPhone {
      babyConnections :: TVar BabyConnections
}

getHomeR :: Handler Html
getHomeR =  do
             babies <- retrieveBabies
             babyName <- lookupSession "babyName"
             ((_, babyWidget), babyEncType) <- generateFormGet $ babyForm babyName
             ((_, parentWidget), parentEncType) <- generateFormGet $ parentForm babies
             defaultLayout $(whamletFile "home.html")


getBabyR :: Handler Html
getBabyR = do
  ((result, _), _) <- runFormGet $ babyForm (Just "baby")
  let babyName = case result of
                    FormSuccess n -> n
                    _ -> "baby"
  setSession "babyName" babyName
  defaultLayout $ do
             $(whamletFile "baby.html")
             toWidget $(juliusFile "baby.js")
             toWidget $(juliusFile "common.js")
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"

getParentR :: Handler Html
getParentR = do
  ((result, parentWidget), parentEncType) <- retrieveBabies >>= runFormGet . parentForm 
  let babyName = case result of
                   FormSuccess n -> n
                   _ -> "baby"
  defaultLayout $ do
               $(whamletFile "parent.html")
               toWidget $(juliusFile "parent.js")
               toWidget $(juliusFile "common.js")
               addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"


getBabiesR :: Handler TypedContent
getBabiesR = do retrieveBabies >>= selectRep . provideRep . returnJson


babyWaiting :: BabyName -> WebSocketsT Handler ()
babyWaiting name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  $logDebug $ "waiting Baby got address: " <> (T.pack . show $ addr)
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
    updateConnections :: BabyPhone -> SockAddr ->  BabyConnection -> SomeException -> WebSocketsT Handler ()
    updateConnections y addr connection e = do
                     $logDebug $ "Baby end communication exited with exception: " <> (T.pack . show $ e)
                     liftIO . atomically $ modifyTVar' (babyConnections y)
                          (\bcs -> dropBabyConnection bcs addr (name,connection))


connectBaby :: BabyName -> WebSocketsT Handler ()
connectBaby name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  $logDebug $ "connect Baby got address: " <> (T.pack . show $ addr)
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
      printException e = $logDebug $ "Parent end communication exited with exception: " <> (T.pack . show $ e)

    
    
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
  BabyPhone connections <- getYesod
  address <- getClientAddress
  liftIO . atomically $ fmap fst . flip getBabies address <$> readTVar connections

main :: IO ()
main = b >>= warp 3000 
    where
      b = BabyPhone <$> atomically (newTVar emptyConnections)

getClientAddress :: Handler SockAddr
--getClientAddress = filterPort . remoteHost <$> waiRequest
getClientAddress = return $ SockAddrUnix ""

filterPort :: SockAddr -> SockAddr
filterPort (SockAddrInet _ address) = SockAddrInet (PortNum 0) address
filterPort (SockAddrInet6 _ flow address scope) = SockAddrInet6 (PortNum 0) flow address scope
filterPort s = s

