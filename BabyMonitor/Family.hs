module BabyMonitor.Family  where

import ClassyPrelude

import qualified Data.Map as M


import BabyMonitor.Client as Client
import qualified BabyMonitor.Types as T
import BabyMonitor.Types ( ClientId, FamilyId, Family(..), ClientInstance, ServerClientMessage(..) )
import qualified BabyMonitor.ClientMap as ClientMap




make :: FamilyId -> Family
make fid = Family fid M.empty M.empty

babyCount :: Family -> Int
babyCount = length . babiesOnline


babyDiff :: Family -> Family -> Int
babyDiff f1 f2 = babyCount f1 - babyCount f2

sendBabies :: ClientInstance -> Family -> IO ()
sendBabies client fmly =
  let
    message = BabiesOnline . fmap T.clientId . babiesOnline $ fmly
  in void . send message $ client

forwardMessage :: ClientInstance -> Text -> ClientId -> Family -> IO ()
forwardMessage src msg destId fmly =
  let
    message = MessageFromClient (T.clientId src) msg
    mdestination = ClientMap.lookupInstance destId . clients $ fmly
    failure = void $ Client.send (NoSuchClient destId) src
  in
    case mdestination of
      Nothing -> failure
      Just destination -> Client.send message destination
                          >>= flip unless failure

modifyBabies :: T.BabyOperation -> ClientInstance -> T.BabyName -> Family -> (IO (), Family)
modifyBabies op client baby fmly = 
  let
    newFmly = op baby client fmly
    message = BabiesOnline . fmap T.clientId . babiesOnline $ newFmly
    action = mapM_ (sendBroadcast message) . clients $ newFmly
  in (action, newFmly)
     
addBaby :: T.BabyOperation
addBaby name client fmly = fmly { babiesOnline = M.insert name client (babiesOnline fmly) }

-- We silently ignore non authorized requests
removeBaby :: T.BabyOperation
removeBaby name client fmly = fmly { babiesOnline = M.update removeIfPermitted name (babiesOnline fmly) }
    where
      removeIfPermitted owner = if T.clientId client == T.clientId owner
                                then Nothing -- Delete
                                else Just owner

deleteInstance :: ClientId -> Family -> Maybe Family
-- Explicit pattern matching on purpose - this code should break on addition of fields, so Server.cleanup gets checked.
deleteInstance cid family@(Family _ clients' babiesOnline') = 
    let
        newClients = ClientMap.deleteInstance cid clients'
        newBabiesOnline = M.fromAscList
                          . filter ((/= cid) . T.clientId . snd)
                          . M.toAscList $ babiesOnline'
                          
    in
      if null newClients then Nothing else Just family { clients = newClients
                                                   , babiesOnline = newBabiesOnline
                                                   }
  

                                    
