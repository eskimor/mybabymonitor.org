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

instance RenderMessage BabyPhone FormMessage where
    renderMessage _ _ = defaultFormMessage


data BabyPhone = BabyPhone {
      babyConnections :: TVar BabyConnections
}

getHomeR :: Handler Html
getHomeR =  do
             address <- getClientAddress
             BabyPhone connections <- getYesod
             babies <- liftIO . atomically $ fmap fst . flip getBabies address <$> readTVar connections 
             defaultLayout $(whamletFile "home.html")


getBabyR :: Handler Html
getBabyR = defaultLayout $ do
             $(whamletFile "baby.html")
             toWidget $(juliusFile "baby.js")
             toWidget $(juliusFile "common.js")
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"

getParentR :: Handler Html
getParentR = defaultLayout $ do
             $(whamletFile "parent.html")
             toWidget $(juliusFile "parent.js")
             toWidget $(juliusFile "common.js")
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"


getBabiesR :: Handler TypedContent
getBabiesR = do
  address <- getClientAddress
  BabyPhone connections <- getYesod
  babies <- liftIO . atomically $ do
            c <- readTVar connections
            return $ fmap fst . getBabies c $ address
  selectRep $ do
               provideRep $ return [shamlet| $forall baby <- babies
                                                     <option value='#{baby}'>#{baby}
                            |]
               provideRep $ returnJson babies


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
  webSock <- ask
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

