module BabyMonitor.Server where

import ClassyPrelude
import Data.Aeson as Aeson
import Data.Map.Strict as M
import qualified Network.WebSockets as WS
import System.Mem.Weak
import Data.Maybe


import BabyMonitor.Types
import qualified  BabyMonitor.Family as Family
import BabyMonitor.Client as Client
import BabyMonitor.ClientMap as ClientMap
import qualified BabyMonitor.UId as UId


init :: IO (TVar Server)
init = atomically . newTVar $ BabyMonitor.Server.make

-- Create a client instance for a newly connected client
-- Just pass Nothing for any maybe you don't have yet.
makeClient :: WS.Connection -> Maybe DeviceId -> Maybe FamilyId -> TVar Server -> IO ClientInstance
makeClient conn' mdid mfid tserv = do
  did <- case mdid of
           Nothing -> UId.make
           Just did' -> return did'
  conn <- mkWeakPtr conn' (cleanupIO mfid did tserv) -- WARNING: May not be safe. See the Warning in: http://haddocks.fpcomplete.com/fp/7.8/20140916-162/base/System-Mem-Weak.html#t:Weak
  atomically $ do
    serv <- readTVar 
    let (client, serv') =  case mfid of
                           Nothing -> makeSingleClientInstance serv conn did
                           Just fid -> makeFamilyClientInstance serv conn did fid
    writeTVar tserv serv'
    return client


-- To be used from /makeFamily
makeFamilyId :: TVar Server -> IO FamilyId
makeFamilyId _ = UId.make


handleMessage :: ClientInstance -> Maybe FamilyId ->  LByteString -> TVar Server -> IO ()
handleMessage client mfmly rmsg tserv = do
  action <- atomically $ do
    serv <- readTVar tserv
    let mmsg = receive rmsg
    let invalidMessage = InvalidMessage . decodeUtf8 rmsg
    let notPermitted = NotPermitted "Single clients are currently not allowed to do anything!"
    let (action', serv') =
          case mmsg of
            Nothing -> ( Client.send invalidMessage client
                       , serv
                       )
            Just msg -> case mfmly of
                          Nothing -> ( Client.send notPermitted client, serv )
                          Just fmly -> handleMessageFamily client fmly msg serv
    writeTVar tserv serv'
    return action'
  action

-- There is nothing to do, but delete the Client id in the session and have the client reconnect. The GC via cleanup will take care of the rest.
declineInvitation :: ClientId -> Server -> Server
declineInvitation _ serv = serv

-- Private - not to be exported:
make :: Server
make =  Server M.empty M.empty M.empty 0 0



receive :: LByteString -> Maybe ClientServerMessage
receive = Aeson.decode

makeSingleClientInstance :: Weak WS.Connection -> DeviceId -> Server -> (ClientInstance, Server)
makeSingleClientInstance conn did serv = 
    second (\s -> serv { singles = s }) . ClientMap.makeInstance conn did . singles $ serv
  -- over _2 (flip (set singles) serv) . ClientMap.makeInstance conn did . _singles $ serv -- With lenses it would be a little more elegant.

makeFamilyClientInstance :: Weak WS.Connection -> DeviceId -> FamilyId -> Server -> (ClientInstance, Server)
makeFamilyClientInstance conn did fid serv = 
    let
        mfmly = M.lookup fid . families $ serv
        family = case mfmly of
                   Nothing -> Family.make fid
                   Just fmly -> fmly
        (client, newClients) = ClientMap.makeInstance conn did . clients $ family
        newFamily = family { clients = newClients }
    in
      (client, updateFamily newFamily serv)

handleMessageFamily :: ClientInstance -> FamilyId -> ClientServerMessage -> Server -> (IO (), Server)
handleMessageFamily source fid msg serv =
    let
      fmly = fromJust . M.lookup fid . families $ serv -- If family does not exist - something is really wrong.
    in
        case msg of
          InviteClient devId -> inviteFamilyMember source fmly devId serv
          AnnounceBaby name -> announceBaby source name fmly serv
          RemoveBaby name -> removeBaby source name fmly serv
          GetBabiesOnline -> sendBabies source fmly serv
                             
-- Handle client requests: 

inviteFamilyMember :: ClientInstance -> Family -> DeviceId -> Server -> (IO (), Server)
inviteFamilyMember source fid inviteeId serv =
    let
        invitation = M.lookup inviteeId . invitations $ serv
        msingle = if isNothing invitation -- Can only be invited once.
                  then M.lookup inviteeId . singles $ serv
                  else Nothing
        action = case msingle of
                   Just single -> Client.sendBroadcast
                                    (HandleInvitation . clientId $ source)
                                    msingle
                   Nothing ->  Client.send
                                 (InvitedClientNotFound inviteeId)
                                 source
        newInvitations = if isJust msingle
                         then M.insert inviteeId fid (invitations serv)
                         else invitations serv
    in 
      (action, serv { invitations = newInvitations })

announceBaby :: ClientInstance -> BabyName -> Family -> Server -> (IO (), Server)
announceBaby = modifyBabies Family.addBaby
               
removeBaby :: ClientInstance -> BabyName -> Family -> Server -> (IO (), Server)
removeBaby = modifyBabies Family.removeBaby


sendBabies :: ClientInstance -> Family -> Server -> (IO (), Server)
sendBabies client fmly serv =
  let
    message = BabiesOnline . fmap clientId . babiesOnline $ fmly
  in (void . send message $ client, serv)
     
modifyBabies :: BabyOperation -> ClientInstance -> BabyName -> Family -> Server -> (IO (), Server)
modifyBabies op client baby fmly serv = 
  let
    newFmly = op baby client fmly
    message = BabiesOnline . fmap clientId . babiesOnline $ newFmly
    action = mapM_ (sendBroadcast message) . clients $ newFmly
    newBabyCount = (length . babiesOnline $ newFmly) - (length . babiesOnline $ fmly)
    (counterAction, newServ) = updateBabyCounter (+newBabyCount) .
                               updateFamily newFmly $ serv
  in (action >> counterAction, newServ)



updateFamily :: Family -> Server -> Server
updateFamily fmly serv = serv { families = M.insert (familyId fmly) fmly (families serv) }

updateBabyCounter :: (Int -> Int) -> Server -> (IO (), Server)
updateBabyCounter f serv = 
    let
        newValue = f . babyCount $ serv
        oldValue = lastSentBabyCount serv
        message = BabyCount newValue
        sendUpdate = abs ((newValue - oldValue)/oldValue) > 0.1
        allClients = singles serv <> (mconcat . fmap clients . M.elems . families $ serv)
        action = if sendUpdate 
                 then mapM_ (Client.sendBroadcast message) allClients
                 else return ()
        newServ = serv { babyCount = newValue
                       , lastSentBabyCount = if sendUpdate
                                             then newValue
                                             else oldValue
                       }
    in
     (action, newServ)

cleanupIO :: Maybe FamilyId -> ClientId -> TVar Server -> IO ()
cleanupIO mfid cid = atomically . cleanup mfid cid

cleanup :: Maybe FamilyId -> ClientId -> TVar Server -> STM ()
cleanup mfid cid tserv = do
  -- Explicit pattern matching on purpose - This code should break on addition of new fields, so it will be adapted! - Otherwise risk of memory leaks.
  serv@(Server singles' families' invitations' _ _) <- readTVar tserv
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
