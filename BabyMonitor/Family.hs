module BabyMonitor.Family where

import ClassyPrelude

import Data.Time.Clock.POSIX
import qualified Data.Map as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar


import BabyMonitor.UId as UId
import BabyMonitor.Client as Client
import BabyMonitor.Types








-- Only singles can be invited to a family. 
inviteFamilyMember :: ClientInstance -> Family ->  TVar Families -> DeviceId -> STM ()
inviteFamilyMember source family families' destination = do
  families <- readTVar families'
  let single = M.lookup destination . singles $ families
  let invitation = M.lookup destination . invitations $ families
  case single of
    Nothing -> Client.send (InvitedClientNotFound destination) source
    Just member -> case invitation of
                     Nothing -> Client.sendBroadcast member (HandleInvitation (clientId source))
                     Just _ -> Client.send (InvitedClientNotFound destination) source -- A client can only be invited once.

  let newInvitations = M.insert (deviceId member) (familyId family) (invitations families)
                       
  writeTVar families' families { invitations = newInvitations}

declineInvitation :: TVar Families -> ClientInstance -> STM ()
declineInvitation source families' = do
  families <- readTVar families'
  let sourceId = devicePart . clientId $ source
  let newInvitations = M.delete sourceId . invitations $ families
  let (single, newSingles) =  removeSingle sourceId . singles $ families
  case single of
    Nothing -> return ()
    Just s -> Client.sendBroadcast s Reconnect
  writeTVar families' families { invitations = newInvitations, singles = newSingles }

-- Private - not to be exported:


makeClient :: ClientId -> STM Client
makeClient uid = Client uid Nothing False <$> newTQueue



fillId :: TMVar UId -> IO ()
fillId mvar = makeUId >>= atomically . putTMVar mvar

removeSingle :: DeviceId -> Map DeviceId Client -> (Maybe Client, Map DeviceId Client)
removeSingle devId families' = 
                let
                    deleteSingle _ _ = Nothing
                in M.updateLookupWithKey deleteSingle devId (singles families')


sendInvitation :: Family -> Client -> STM ()
sendInvitation = error "Not yet implemented"


modifyTVarSTM :: TVar a -> (a -> STM (b, a)) -> STM b
modifyTVarSTM var f = do
  a <- readTVar var
  (b, a') <- f a
  writeTVar var a'
  return b
