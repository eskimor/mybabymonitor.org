module BabyMonitor.Types where

import ClassyPrelude

import BabyMonitor.UId (UId)

type ClientId = UId 
type FamilyId = UId
type DeviceId = UId
    

type BabyName = Text
    
type ClientMap = Map ClientId Client
type FamilyMap = Map FamilyId Family

data Client = Client {
      deviceId :: ClientId
    , toClient :: [TQueue ByteString] -- As there could be multiple
                                      -- instances of one client (user
                                      -- opens the site in multiple
                                      -- tabs), we need a list of
                                      -- TQueues, with a queue for
                                      -- each instance.
    }


              
data Family = Family {
      familyId :: FamilyId
    , clients :: ClientMap
    , babiesOnline :: Map BabyName Client
    }



data Families = Families {
      singles :: ClientMap -- Clients which are not yet in a family
    , families :: FamilyMap
    , invitations :: Map ClientId FamilyId
    , nextClientId :: TMVar UId
    , nextFamilyId :: TMVar UId
    }
