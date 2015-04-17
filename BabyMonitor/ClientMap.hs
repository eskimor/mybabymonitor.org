module BabyMonitor.ClientMap where
    
import BabyMonitor.Types
import ClassyPrelude
import qualified Data.Map.Strict as M
import Network.WebSockets as WS
import System.Mem.Weak

import qualified  BabyMonitor.Client as Client
import qualified BabyMonitor.UId as UId


lookupClient :: DeviceId -> ClientMap -> Maybe Client
lookupClient = lookup

getAutoComplete :: Text -> ClientMap -> Maybe DeviceId
getAutoComplete start cls = 
  let
      searchMap = snd . M.split start . M.mapKeysMonotonic UId.toUserString $ cls
      results = takeWhile (isPrefixOf start . fst)
                . M.toAscList
                $ searchMap
      getResult ((_, cl):[]) = Just . deviceId $ cl -- start was unique
      getResult _ = Nothing
  in
    if length start >= 4
    then getResult results
    else Nothing

lookupInstance :: ClientId -> ClientMap -> Maybe ClientInstance
lookupInstance (ClientId dp ip) m = lookup dp m >>= lookup ip . instances 

deleteInstance :: ClientId -> ClientMap -> ClientMap
deleteInstance (ClientId dp ip) = M.update (Client.deleteInstance ip) dp

makeInstance :: Weak WS.Connection -> DeviceId -> ClientMap -> (ClientInstance, ClientMap)
makeInstance conn did m =
  let
    mclient = lookupClient did m
    (newInstance, newClient) = case mclient of
                  Nothing -> Client.make conn did
                  Just client -> Client.makeInstance conn client
  in
    (newInstance, M.insert did newClient m)
