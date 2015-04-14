module BabyMonitor.Server where

import ClassyPrelude
import Data.Aeson as Aeson
import Control.Lens


import BabyMonitor.Types
import BabyMonitor.Family
import BabyMonitor.Client
import BabyMonitor.ClientMap as ClientMap
import BabyMonitor.UId


init :: IO (TVar Server)
init = make >>= atomically . newTVar

-- Create a client instance for a newly connected client
-- Just pass Nothing for any maybe you don't have yet.
makeClient :: TVar Server -> WS.Connection -> Maybe DeviceId -> Maybe FamilyId -> IO ClientId
makeClient tserv conn' mdid mfid = do
  conn <- mkWeakPtr conn' (cleanupIO ...) -- WARNING: May not be safe. See the Warning in: http://haddocks.fpcomplete.com/fp/7.8/20140916-162/base/System-Mem-Weak.html#t:Weak
  did <- case mdid of
           Nothing -> UId.make
           Just did' -> return did'
  atomically $ case mfid of
                 Nothing -> makeSingleClientInstance serv conn did
                 Just fid -> makeFamilyClientInstance serv conn did fid


-- To be used from /createFamily
createFamily :: TVar Server -> STM FamilyId
createFamily = readTMVar . nextFamilyId <=< readTVar 


handleMessage :: ClientId -> Maybe FamilyId -> TVar Server -> LByteString -> IO ()
handleMessage srcId mfmly tserv rmsg = do
  action <- atomically $ do
    serv <- readTVar tserv
    let client = 
    let mmsg = receive rmsg
    case mmsg of
      Nothing -> return Client.send (InvalidMessage (decodeUtf8 rmsg))
      Just msg -> case mfmly of
                    Nothing -> handleMessageSingle srcId serv msg
                    Just fmly -> handleMessageFamily srcId fmly serv msg
  action
  


-- Private - not to be exported:
make :: IO Server
make = do
  (id1, id2) <- (,) <$> UId.make <*> UId.make
  clients <- atomically
             $ Server M.empty M.empty M.empty 0
             <$> newTMVar id1
             <*> newTMVar id2
  forkIO $ forever $ fillId (nextClientId clients)
  forkIO $ forever $ fillId (nextFamilyId clients)
  return clients


handleMessageSingle :: ClientId -> Server -> ClientServerMessage -> STM ()
handleMessage' source  famlies message = do
  case message of
    DeclineInvitation -> 
  case message of
   InviteClient devId -> inviteFamilyMember source family families devId

receive :: ClientInstance -> LByteString -> Maybe ClientServerMessage
receive = Aeson.decode

makeSingleClientInstance :: WS.Connection -> DeviceId -> Server -> (ClientId, Server)
makeSingleClientInstance conn did serv = 
  --(clid, newSingles) = second (\newSingles -> serv { singles = newSingles }) ClientMap.makeInstance conn did (singles serv)
  over _2 (flip (set singles) serv)
           . ClientMap.makeInstance conn did . _singles $ serv

makeFamilyClientInstance :: TVar Server -> WS.Connection -> DeviceId -> FamilyId -> STM ClientId
makeFamilyClientInstance tserv conn did fid= do
  serv <- readTVar tserv
  let mfmly = lookup fid (families serv)
  let family = case mfmly of
    Nothing -> Family.make fid
    Just fmly -> fmly
  (clid, newClients) = ClientMap.makeInstance conn did (clients family)
  let newFamily = family { clients = newClients }
  let newFamilies = M.insert newFamily (families serv)
  writeTVar tserv serv { families = newFamilies }
  return clid

-- Handle client requests: 

inviteFamilyMember :: Server -> ClientInstance -> FamilyId -> DeviceId -> (IO (), Server)
inviteFamilyMember serv source fid inviteeId =
    let
        invitation = M.lookup invitee . invitations $ serv
        msingle = if isNothing invitation -- Can only be invited once.
                  then M.lookup destination . singles $ serv
                  else Nothing
        action = case msingle of
                   Just single -> Client.sendBroadcast
                                    (HandleInvitation . clientId $ source)
                                    msingle
                   Nothing ->  Client.send
                                 (InvitedClientNotFound destination)
                                 source
        newInvitations = if isJust msingle
                         then M.insert inviteeId fid (invitations serv)
                         else invitations serv
    in 
      (action, serv { invitations = newInvitations })

declineInvitation :: Server -> ClientInstance -> Server
declineInvitation server source =
  let
      sourceId = devicePart . clientId $ source
      newInvitations = M.delete sourceId . invitations $ server
      (single, newSingles) =  removeSingle sourceId . singles $ server
  case single of
    Nothing -> return ()
    Just s -> Client.sendBroadcast s Reconnect
  writeTVar families' families { invitations = newInvitations, singles = newSingles }

fillId :: TMVar UId -> IO ()
fillId mvar = UId.make >>= atomically . putTMVar mvar

cleanupIO :: TVar Server -> Maybe FamilyId -> ClientId -> IO ()
cleanupIO = atomically . cleanup

cleanup :: TVar Server -> Maybe FamilyId -> ClientId -> STM ()
cleanup tserv mfid cid = do
  -- Explicit pattern matching on purpose - This code should break on addition of new fields, so it will be adapted! - Otherwise risk of memory leaks.
  serv@(Server singles' families' invitations' _ _ _) <- readTVar tserv
  let newSingles = ClientMap.deleteInstance cid singles'
  let newFamilies = case mfid of
        Nothing -> families'
        Just fid -> M.update (Family.deleteInstance cid) fid families' 
  let newInvitations = M.delete (devicePart cid) invitations'
  writeTVar tserv serv { singles = newSingles
                       , families = newFamilies
                       , invitations = newInvitations
                       }

modifyTVarSTM :: TVar a -> (a -> STM (b, a)) -> STM b
modifyTVarSTM var f = do
  a <- readTVar var
  (b, a') <- f a
  writeTVar var a'
  return b
