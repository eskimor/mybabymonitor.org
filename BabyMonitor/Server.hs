module BabyMonitor.Server (
                           makeClient
                          , makeDeviceId
                          , makeFamilyId
                          , handleMessage
                          , sendId
                          , declineInvitation
                          , acceptInvitation
                          , Server
                          , init
                          , ClientId
                          , DeviceId
                          , FamilyId
                          )
    where

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

makeDeviceId :: IO DeviceId
makeDeviceId = UId.make

-- Create a client instance for a newly connected client
makeClient :: WS.Connection -> DeviceId -> Maybe FamilyId -> TVar Server -> IO ClientInstance
makeClient conn' did mfid tserv = do
  conn <- mkWeakPtr conn' Nothing 
  cl <- atomically $ do
    serv <- readTVar tserv
    let (client, serv') =  case mfid of
                           Nothing -> makeSingleClientInstance conn did serv
                           Just fid -> makeFamilyClientInstance conn did fid serv
    writeTVar tserv serv'
    return client
  addFinalizer conn' (cleanupIO mfid (clientId cl) tserv) -- WARNING: May not be safe. See the Warning in: http://haddocks.fpcomplete.com/fp/7.8/20140916-162/base/System-Mem-Weak.html#t:Weak
  return cl


-- To be used from /makeFamily
makeFamilyId :: IO FamilyId
makeFamilyId = UId.make


sendId :: ClientInstance -> IO ()
sendId client = void $ Client.send (YourId . devicePart . clientId $ client) client

handleMessage :: ClientInstance -> Maybe FamilyId ->  LByteString -> TVar Server -> IO ()
handleMessage client mfmly rmsg tserv = join . atomically $ do
    serv <- readTVar tserv
    let mmsg = receive rmsg
    let invalidMessage = InvalidMessage . decodeUtf8 $ rmsg
    let notPermitted = NotPermitted "Single clients are currently not allowed to do a nything!"
    let (action, serv') =
          case mmsg of
            Nothing -> ( void $ Client.send invalidMessage client
                       , serv
                       )
            Just msg -> case mfmly of
                          Nothing -> (void $ Client.send notPermitted client, serv )
                          Just fmly -> handleMessageFamily client fmly msg serv
    writeTVar tserv serv'
    return action

-- There is nothing to do, but delete the Client id in the session and have the client reconnect. The GC via cleanup will take care of the rest.
declineInvitation :: ClientId -> Server -> Server
declineInvitation _ serv = serv

acceptInvitation :: DeviceId -> TVar Server -> IO (Maybe FamilyId)
acceptInvitation did tserv = atomically $ do
  serv <- readTVar tserv
  let minvitation = M.lookup did (invitations serv)
  -- Invitations won't be touched, cleanup takes care of them. Keeping
  -- the invitation around does not harm, but prevent the client from being
  -- invited again.
  return minvitation
    
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
        family = fromMaybe (Family.make fid) mfmly
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
          AnnounceBaby name -> modifyBabies Family.addBaby source name fmly serv
          RemoveBaby name -> modifyBabies Family.removeBaby source name fmly serv
          GetBabiesOnline -> (Family.sendBabies source fmly, serv)
          MessageToClient clid fmsg -> (Family.forwardMessage source fmsg clid fmly
                                         , serv)
          GetAutoComplete txt -> (sendAutoComplete source txt (singles serv), serv)
                             
-- Handle client requests: 

inviteFamilyMember :: ClientInstance -> Family -> DeviceId -> Server -> (IO (), Server)
inviteFamilyMember source fmly inviteeId serv =
    let
        invitation = M.lookup inviteeId . invitations $ serv
        msingle = if isNothing invitation -- Can only be invited once.
                  then M.lookup inviteeId . singles $ serv
                  else Nothing
        action = case msingle of
                   Just single -> Client.sendBroadcast
                                    (HandleInvitation . clientId $ source)
                                    single
                   Nothing ->  void $ Client.send
                                 (InvitedClientNotFound inviteeId)
                                 source
        newInvitations = if isJust msingle
                         then M.insert inviteeId (familyId fmly) (invitations serv)
                         else invitations serv
    in 
      (action, serv { invitations = newInvitations })


sendAutoComplete :: ClientInstance -> Text -> ClientMap -> IO ()
sendAutoComplete client txt m = do
  let mcompletion = ClientMap.getAutoComplete txt m
  case mcompletion of
    Nothing -> return ()
    Just did -> void $ Client.send (AutoCompleteResult did) client

     
modifyBabies :: BabyOperation -> ClientInstance -> BabyName -> Family -> Server -> (IO (), Server)
modifyBabies op client baby fmly serv = 
  let
    (action, newFmly) = Family.modifyBabies op client baby fmly
    newBabyCount = Family.babyDiff newFmly fmly 
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
        sendUpdate = abs (fromIntegral (newValue - oldValue) / fromIntegral oldValue) > 0.1
        allClients = singles serv <> (mconcat . fmap clients . M.elems . families $ serv)
        action = when sendUpdate $ mapM_ (Client.sendBroadcast message) allClients
        newServ = serv { babyCount = newValue
                       , lastSentBabyCount = if sendUpdate
                                             then newValue
                                             else oldValue
                       }
    in
     (action, newServ)

cleanupIO :: Maybe FamilyId -> ClientId -> TVar Server -> IO ()
cleanupIO mfid cid tserv = join .  atomically . cleanup mfid cid $ tserv

cleanup :: Maybe FamilyId -> ClientId -> TVar Server -> STM (IO ())
cleanup mfid cid tserv = do
  -- Explicit pattern matching on purpose - This code should break on addition of new fields, so it will be adapted! - Otherwise risk of memory leaks.
  serv@(Server singles' families' invitations' _ _) <- readTVar tserv
  let newSingles = ClientMap.deleteInstance cid singles'
  let noInstancesLeft = length newSingles < length singles' 
  let newInvitations = if noInstancesLeft
                       then M.delete (devicePart cid) invitations'
                       else invitations'
                            
  let (action, newServer) = case mfid of
        Nothing -> (return (), serv )
        Just fid ->
          let
              oldFamily = fromJust $ M.lookup fid families' -- fromJust: Exception is fine, it should really not happen that we have an id but no data.
              mNewFamily = Family.deleteInstance cid oldFamily
              babyDiff = case mNewFamily of
                           Nothing -> 0 - Family.babyCount oldFamily
                           Just newFamily -> Family.babyCount newFamily - Family.babyCount oldFamily
              newFamilies = M.update (Family.deleteInstance cid) fid families' 
          in
            updateBabyCounter (+babyDiff) serv { families = newFamilies}
           
  writeTVar tserv newServer { singles = newSingles
                       , invitations = newInvitations
                       }
  return action

modifyTVarSTM :: TVar a -> (a -> STM (b, a)) -> STM b
modifyTVarSTM var f = do
  a <- readTVar var
  (b, a') <- f a
  writeTVar var a'
  return b
