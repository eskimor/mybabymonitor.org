module BabyMonitor.ClientMap where
    
import BabyMonitor.Types
import ClassyPrelude
import qualified Data.Map.Strict as M
import Network.WebSockets as WS

import qualified  BabyMonitor.Client as Client



lookupClient :: DeviceId -> ClientMap -> Maybe Client
lookupClient did m = lookup did m


lookupInstance :: ClientId -> ClientMap -> Maybe ClientInstance
lookupInstance (ClientId dp ip) m = lookup dp m >>= lookup ip . instances 

deleteInstance :: ClientId -> ClientMap -> ClientMap
deleteInstance (ClientId dp ip) m = M.update updateClient dp m
    where
      updateClient cl =
        let
          newInstances = M.delete ip (instances cl)
        in
          if null newInstances
          then Nothing
          else Just cl { instances = newInstances }
        

makeInstance :: WS.Connection -> DeviceId -> ClientMap -> (ClientId, ClientMap)
makeInstance conn did m =
  let
    mclient = lookupClient did m
    (newInstance, newClient) = case mclient of
                  Nothing -> Client.make conn did
                  Just client -> Client.makeInstance conn client
  in
    (clientId newInstance, M.insert did newClient m)
