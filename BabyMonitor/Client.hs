
module BabyMonitor.Client where

import ClassyPrelude

import Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import BabyMonitor.Types
import qualified Data.Map.Strict as M

 

make :: WS.Connection -> DeviceId -> (ClientInstance, Client)
make conn did = (newInstance, Client did (M.singleton 0 newInstance) 1)
    where
      newInstance = ClientInstance (ClientId did 0) conn

makeInstance :: WS.Connection -> Client -> (ClientInstance, Client)
makeInstance conn cl = (newInst
                       , cl {
                          instances = M.insert nextId newInst (instances cl)
                        , nextInstanceId = nextId + 1
                        }
                       )
  where
    nextId = nextInstanceId cl
    newInst = ClientInstance (ClientId (deviceId cl) nextId) conn

send :: ServerClientMessage -> ClientInstance -> STM ()
send msg (ClientInstance _ queue) = undefined

sendBroadcast :: Client -> ServerClientMessage -> STM ()
sendBroadcast (Client _ instances _) msg = mapM_ (send msg) instances
