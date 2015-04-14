module BabyMonitor.ClientMap where
    
import BabyMonitor.Types
import ClassyPrelude
import qualified Data.Map.Strict as M
import Network.WebSockets as WS
import System.Mem.Weak

import qualified  BabyMonitor.Client as Client



lookupClient :: DeviceId -> ClientMap -> Maybe Client
lookupClient did m = lookup did m


lookupInstance :: ClientId -> ClientMap -> Maybe ClientInstance
lookupInstance (ClientId dp ip) m = lookup dp m >>= lookup ip . instances 

deleteInstance :: ClientId -> ClientMap -> ClientMap
deleteInstance (ClientId dp ip) m = M.update (Client.deleteInstance ip) dp m

makeInstance :: Weak WS.Connection -> DeviceId -> ClientMap -> (ClientInstance, ClientMap)
makeInstance conn did m =
  let
    mclient = lookupClient did m
    (newInstance, newClient) = case mclient of
                  Nothing -> Client.make conn did
                  Just client -> Client.makeInstance conn client
  in
    (newInstance, M.insert did newClient m)
