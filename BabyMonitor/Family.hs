module BabyMonitor.Family where

import ClassyPrelude

import Data.Time.Clock.POSIX
import qualified Data.Map as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar


import BabyMonitor.UId as UId
import BabyMonitor.Client as Client
import BabyMonitor.Types
import qualified BabyMonitor.ClientMap as ClientMap




make :: FamilyId -> Family
make fid = Family fid M.empty M.empty


deleteInstance :: ClientId -> Family -> Maybe Family
-- Explicit pattern matching on purpose - this code should break on addition of fields, so Server.cleanup gets checked.
deleteInstance cid family@(Family _ clients' babiesOnline') = 
    let
        newClients = ClientMap.deleteInstance cid clients'
        newBabiesOnline = M.fromAscList
                          . filter ((/= cid) . clientId . snd)
                          . M.toAscList $ babiesOnline'
                          
    in
      if null newClients then Nothing else Just family { clients = newClients
                                                   , babiesOnline = newBabiesOnline
                                                   }
  

sendInvitation :: Family -> Client -> STM ()
sendInvitation = error "Not yet implemented"


