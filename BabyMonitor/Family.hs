module BabyMonitor.Family where

import ClassyPrelude

import Data.Time.Clock.POSIX
import qualified Data.Map as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar


import BabyMonitor.UId
import BabyMonitor.Client





initFamilies :: IO (TVar Families)
initFamilies = makeFamilies >>= atomically . newTVar

initClient :: TVar Families -> STM Client
initClient cs = do
  clients <- readTVar cs
  client <- takeTMVar (nextClientId clients) >>= makeClient
  let singles' = M.insert (clientId client) client (singles clients)
  writeTVar cs (clients { singles = singles' } )
  return client


-- Only singles can be invited to a family. If the given client id is no longer single this function does nothing and therefore also returns Nothing to make that clear.
inviteFamilyMember :: Family -> ClientId -> Families -> STM (Maybe Families)
inviteFamilyMember family clientId clients =
  let
    (single, clients') = removeSingle clientId clients
  in
    return $ do  -- To be continued ... 
      client <- single
      return $ clients' { invitations = M.insert (familyId family) clientId (invitations clients') }

-- Private - not to be exported:

makeFamilies :: IO Families
makeFamilies = do
  (id1, id2) <- (,) <$> makeUId <*> makeUId
  clients <- atomically
             $ Families M.empty M.empty M.empty
             <$> newTMVar id1
             <*> newTMVar id2
  forkIO $ forever $ fillId (nextClientId clients)
  forkIO $ forever $ fillId (nextFamilyId clients)
  return clients

makeClient :: ClientId -> STM Client
makeClient uid = Client uid Nothing False <$> newTQueue



fillId :: TMVar UId -> IO ()
fillId mvar = makeUId >>= atomically . putTMVar mvar

removeSingle :: ClientId -> Families -> (Maybe Client, Families)
removeSingle clientId clients = 
                let
                    (client, singles') = M.updateLookupWithKey deleteSingle clientId (singles clients)
                    deleteSingle _ _ = Nothing
                in (client, clients { singles = singles'})


sendInvitation :: Family -> Client -> STM ()
sendInvitation = error "Not yet implemented"


modifyTVarSTM :: TVar a -> (a -> STM (b, a)) -> STM b
modifyTVarSTM var f = do
  a <- readTVar var
  (b, a') <- f a
  writeTVar var a'
  return b
