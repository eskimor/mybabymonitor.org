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
import Data.Aeson

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
  connections <- liftIO . atomically $ babyConnections y
  addr <- lift getClientAddress
  let babies = getBabies connections addr
  (input, output) <- atomically $ (,) <$> newTQueue <*> newTQueue
  let newConnections =
                   M.insertWith -- Replace any old connection
                        (\new old -> new ++ filter ((/= name).fst) old)
                        addr
                        [(name, (input, output))]
                        connections
  liftIO . atomically $ writeTVar (babyConnections y) newConnections

connectBaby :: BabyName -> WebSocketsT Handler ()
connectBaby name = do
  y <- lift getYesod
  connections <- liftIO . atomically $ babyConnections y
  addr <- lift getClientAddress
  let (mBaby, newConnections) = extractBaby connections addr name
  case mBaby of
    Nothing -> sendTextData $ "{\"error\" : \"You don't have a baby named" <> name <> "!\"}"
    baby -> do
      baby >> sinkWSText
    
    
getBabyOpenChannelR :: BabyName -> Handler ()
getBabyOpenChannelR = webSockets . babyWaiting 

getBabyConnectChannelR :: BabyName -> Handler ()
getBabyConnectChannelR = undefined


main :: IO ()
main = b >>= warp 3000 
    where
      b = BabyPhone <$> atomically $ newTVar M.empty

getClientAddress :: Handler SockAddr
getClientAddress = return $ remoteHost <$> waiRequest 
                   
