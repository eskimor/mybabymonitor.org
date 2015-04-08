module BabyMonitor.Types where

import ClassyPrelude

import BabyMonitor.UId (UId)

type ClientId = UId 
type FamilyId = UId
    

type BabyName = Text
    
type ClientMap = Map ClientId Client
type FamilyMap = Map FamilyId Family

data Client = Client {
      clientId :: ClientId
    , toClient :: TQueue ByteString
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
