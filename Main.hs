{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import BabyCommunication




mkYesod "BabyPhone" [parseRoutes|
                      / HomeR GET
                      /baby BabyR GET
                      /parent ParentR GET
                      /babies BabiesR GET
                      /babyOpenChannel/#Text BabyOpenChannelR GET -- Opened by the baby
                      /babyConnectChannel/#Text BabyConnectChannelR GET -- Connected by the parent
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
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"

getParentR :: Handler Html
getParentR = defaultLayout $ do
             $(whamletFile "parent.html")
             toWidget $(juliusFile "parent.js")
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"


getBabiesR :: Handler RepJson
getBabiesR = do
  address <- getClientAddress
  BabyPhone connections <- getYesod
  liftIO . atomically $ getBabies <$> connections


babyWaiting :: BabyName -> WebSocketsT Handler ()
babyWaiting name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  atomically $ do
    connections <-  readTVar $ babyConnections y
    (connection, connections') <- newBaby connections addr name
    putTVar (babyConnections y) $ connections'
  race_
                   (forever $ finally writer updateConnections)
                   (forever $ finally reader updateConnections)
    where
      writer = atomically . babySend connection <$> receiveData
      reader = atomically . babyReceive connection >>= sendTextData 
      updateConnections =
          atomically $ modifyTVar' (babyConnections y)
                         (\bcs -> dropBabyConnection bcs addr connection)

connectBaby :: BabyName -> WebSocketsT Handler ()
connectBaby name = do
  y <- lift getYesod
  addr <- lift getClientAddress
  atomically $ do
    connections <- readTVar $ babyConnections y
    let (mBaby, connections') = popBabyConnection connections addr name
    putTVar (babyConnections y) connections'
    
  case mBaby of
    Nothing -> sendTextData $ "{\"error\" : \"You don't have a baby named" <> name <> "!\"}"
    Just (_, connection) -> race_
                   (forever $ atomically . parentSend connection <$> receiveData)
                   (forever $ atomically . parentReceive connection >>= sendTextData)
      
    
    
getBabyOpenChannelR :: BabyName -> Handler ()
getBabyOpenChannelR = webSockets . babyWaiting 

getBabyConnectChannelR :: BabyName -> Handler ()
getBabyConnectChannelR = webSockets . connectBaby


main :: IO ()
main = b >>= warp 3000 
    where
      b = BabyPhone <$> atomically $ newTVar M.empty

getClientAddress :: Handler SockAddr
getClientAddress = return $ remoteHost <$> waiRequest 
                   
