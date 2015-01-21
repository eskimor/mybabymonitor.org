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
import Network.Socket (SockAddr)
import Yesod.WebSockets
import qualified Network.WebSockets as WS
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Aeson
import Control.Exception.Base (finally)
import Network.Wai (remoteHost)
import Control.Monad 
import Data.Monoid ((<>))
import Yesod.Core.Json
import Control.Monad.Reader (ask)

import BabyCommunication




mkYesod "BabyPhone" [parseRoutes|
                      / HomeR GET
                      /baby BabyR GET
                      /parent ParentR GET
                      /babies BabiesR GET
                      /babyOpenChannel/#T.Text BabyOpenChannelR GET -- Opened by the baby
                      /babyConnectChannel/#T.Text BabyConnectChannelR GET -- Connected by the parent
|]

instance Yesod BabyPhone

instance RenderMessage BabyPhone FormMessage where
    renderMessage _ _ = defaultFormMessage


data BabyPhone = BabyPhone {
      babyConnections :: TVar BabyConnections
}

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]


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


getBabiesR :: Handler Value
getBabiesR = do
  address <- getClientAddress
  BabyPhone connections <- getYesod
  babies <- liftIO . atomically $ do
            c <- readTVar connections
            return $ fmap fst . getBabies c $ address
  returnJson babies


babyWaiting :: BabyName -> WebSocketsT Handler ()
babyWaiting name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  connection <- liftIO . atomically $ do
    connections <-  readTVar $ babyConnections y
    (connection, connections') <- newBaby connections addr name
    writeTVar (babyConnections y) connections'
    return connection
  websock <- ask
  let writer = atomically . babySend connection <$> WS.receiveData websock
  let reader = (atomically . babyReceive) connection >>= WS.sendTextData websock
  let updateConnections = atomically $
                          modifyTVar' (babyConnections y)
                          (\bcs -> dropBabyConnection bcs addr (name,connection))
  race_
                   (liftIO $ finally (forever writer) updateConnections)
                   (liftIO $ finally (forever reader) updateConnections)

connectBaby :: BabyName -> WebSocketsT Handler ()
connectBaby name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  mBaby <- liftIO . atomically $ do
    connections <- readTVar $ babyConnections y
    let (mBaby, connections') = popBabyConnection connections addr name
    writeTVar (babyConnections y) connections'
    return mBaby
  webSock <- ask
  case mBaby of
    Nothing -> sendTextData $ "{\"error\" : \"You don't have a baby named" <> name <> "!\"}"
    Just connection -> liftIO $ race_
                   (forever $ atomically . parentSend connection <$> WS.receiveData webSock)
                   (forever $ (atomically . parentReceive) connection >>= WS.sendTextData webSock)
      
    
    
getBabyOpenChannelR :: BabyName -> Handler ()
getBabyOpenChannelR = webSockets . babyWaiting 

getBabyConnectChannelR :: BabyName -> Handler ()
getBabyConnectChannelR = webSockets . connectBaby


main :: IO ()
main = b >>= warp 3000 
    where
      b = BabyPhone <$> atomically (newTVar emptyConnections)

getClientAddress :: Handler SockAddr
getClientAddress = remoteHost <$> waiRequest 

{--ourFinally :: WebSocketsT Handler a -> WebSocketsT Handler b -> WebSocketsT Handler ()
ourFinally a b = do
  ma <- a
  mb <- b
  liftIO $ finally (return ma) (return mb)
    

--}
