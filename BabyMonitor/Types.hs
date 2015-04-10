module BabyMonitor.Types where

import ClassyPrelude

import BabyMonitor.UId (UId)

type DeviceId = UId

data ClientId = ClientId {
      devicePart :: DeviceId,
      instancePart :: Int
    }
                
type FamilyId = UId
    

type BabyName = Text
    
type ClientMap = Map ClientId ClientInstance
type FamilyMap = Map FamilyId Family

data Client = Client {
      deviceId :: DeviceId
    , instances :: [ClientInstance]
    }

data ClientInstance = ClientInstance {
      clientId :: ClientId
    , toClient :: [TQueue ByteString] -- As there could be multiple
    }
              
data Family = Family {
      familyId :: FamilyId
    , clients :: ClientMap
    , babiesOnline :: Map BabyName ClientInstance
    }



data Families = Families {
      singles :: Map DeviceId Client -- Clients which are not yet in a family
    , families :: FamilyMap
    , invitations :: Map ClientId FamilyId
    , nextClientId :: TMVar UId
    , nextFamilyId :: TMVar UId
    }
