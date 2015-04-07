module Client (makeClients
              , Client
              , Clients
              , Family
              , makeClients
              , newClient
              ) where

import ClassyPrelude

import Data.Time.Clock.POSIX
import qualified Data.Map as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar


import BabyMonitor.UId

type FamilyId = UId
type ClientId = UId 
    
type ClientMap = Map ClientId Client
type FamilyMap = Map FamilyId Family

type BabyName = Text

data Client = Client {
      clientId :: ClientId
    , babyName :: Maybe Text
    , babyStarted :: Bool
    , toClient :: TQueue Text
    }

data Family = Family {
      familyId :: FamilyId
    , clients :: ClientMap
    , babiesOnline :: Map BabyName Client
    }



data Clients = Clients {
      singles :: ClientMap -- Clients which are not yet in a family
    , families :: FamilyMap
    , invitations :: Map ClientId FamilyId
    , nextClientId :: TMVar UId
    , nextFamilyId :: TMVar UId
    }


makeClients :: IO Clients
makeClients = do
  (id1, id2) <- (,) <$> makeUId <*> makeUId
  clients <- atomically
             $ Clients M.empty M.empty M.empty
             <$> newTMVar id1
             <*> newTMVar id2
  forkIO $ forever $ fillId (nextClientId clients)
  forkIO $ forever $ fillId (nextFamilyId clients)
  return clients

  
newClient :: Clients -> STM (Client, Clients)
newClient cs = do
  client <- takeTMVar (nextClientId cs) >>= makeClient
  let clients' = M.insert (clientId client) client (singles cs)
  return (client, cs { singles = clients'})


-- Only singles can be invited to a family. If the given client id is no longer single this function does nothing and therefore also returns Nothing to make that clear.
inviteFamilyMember :: Family -> ClientId -> Clients -> STM (Maybe Clients)
inviteFamilyMember family clientId clients =
  let
    (single, clients') = removeSingle clientId clients
  in
    return $ do  -- To be continued ... 
      client <- single
      return $ clients' { invitations = M.insert (familyId family) clientId (invitations clients') }

-- Private - not to be exported:

makeClient :: ClientId -> STM Client
makeClient uid = Client uid Nothing False <$> newTQueue



fillId :: TMVar UId -> IO ()
fillId mvar = makeUId >>= atomically . putTMVar mvar

removeSingle :: ClientId -> Clients -> (Maybe Client, Clients)
removeSingle clientId clients = 
                let
                    (client, singles') = M.updateLookupWithKey deleteSingle clientId (singles clients)
                    deleteSingle _ _ = Nothing
                in (client, clients { singles = singles'})


sendInvitation :: Family -> Client -> STM ()
sendInvitation = error "Not yet implemented"
