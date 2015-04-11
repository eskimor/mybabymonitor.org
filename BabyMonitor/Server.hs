module BabyMonitor.Server where

import ClassyPrelude
import Data.Aeson as Aeson


import BabyMonitor.Types
import BabyMonitor.Family
import BabyMonitor.Client
import BabyMonitor.ClientMap as ClientMap

init :: IO (TVar Server)
init = make >>= atomically . newTVar


makeClient :: TVar Server -> WS.Connection -> Maybe DeviceId -> Maybe FamilyId -> STM ClientId
makeClient tserv conn mdid mfid = do
  return case mdid of
    Nothing -> makeClient' serv conn
    Just did -> case mfid of
                  Nothing -> makeSingleClientInstance serv conn did
                  Just fid -> makeFamilyClientInstance serv conn did fid

makeClient' :: TVar Server -> WS.Connection -> STM ClientId
makeClient' tserv conn = do
  serv <- readTVar tserv
  client <- takeTMVar (nextClientId serv) >>= Client.make conn
  let newSingles = M.insert (deviceId client) client (singles serv)
  writeTVar tserv clients { singles = newSingles }
  return $ clientId . head . instances client

makeSingleClientInstance :: TVar Server -> WS.Connection -> DeviceId -> STM ClientId
makeSingleClientInstance tserv conn did = do
  serv <- readTVar tserv
  (clid, newSingles) = ClientMap.makeInstance conn did (singles serv)
  writeTVar tserv serv { singles = newSingles }
  return clid

makeFamilyClientInstance :: TVar Server -> WS.Connection -> DeviceId -> FamilyId -> STM ClientId

makeFamilyClientInstance tserv conn did fid= do
  serv <- readTVar tserv
  -- ... to be continued - lookup family, create new if necessary - add client
  (clid, newClients) = ClientMap.makeInstance conn did (singles serv)
                       
  

handleMessage :: ClientId -> Maybe FamilyId -> TVar Server -> LByteString -> STM ( IO () )
handleMessage srcId mfmly tserv rmsg = do
  serv <- readTVar tserv
  let client = 
  let mmsg = receive rmsg
  case mmsg of
    Nothing -> return Client.send (InvalidMessage (decodeUtf8 rmsg))
    Just msg -> case mfmly of
                  Nothing -> handleMessageSingle srcId serv msg
                  Just fmly -> handleMessageFamily srcId fmly serv msg



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
