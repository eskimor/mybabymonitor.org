{-# LANGUAGE DeriveGeneric #-}
module BabyMonitor.Types where

import ClassyPrelude
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Network.WebSockets as WS

import BabyMonitor.UId

type DeviceId = UId

data ClientId = ClientId {
      devicePart :: DeviceId,
      instancePart :: Int
    } deriving (Show, Eq, Ord)
              
                
type FamilyId = UId
    

type BabyName = Text
    
type ClientMap = Map DeviceId Client
type FamilyMap = Map FamilyId Family

data Client = Client {
      deviceId :: DeviceId
    , instances :: Map Int ClientInstance
    , nextInstanceId :: Int
    }

data ClientInstance = ClientInstance {
      clientId :: ClientId
    , toClient :: WS.Connection
    }

data ServerClientMessage =
    HandleInvitation ClientId
  | InvitedClientNotFound DeviceId 
  | NotInFamily Text
  | BabiesOnline (M.Map BabyName ClientId)
  | DuplicateBaby Text
  | NotPermitted Text
  | MessageFromClient ClientId Text
  | NoSuchClient ClientId
  | InvalidMessage Text
  | AutoCompleteResult DeviceId
  | BabyCount Int
  | YourId DeviceId
  | Reconnect
   deriving (Generic, Show)

data ClientServerMessage =
    InviteClient DeviceId
  | AnnounceBaby Text
  | RemoveBaby Text
  | GetBabiesOnline
  | MessageToClient ClientId
  | GetAutoComplete Text
  deriving (Generic, Show)
              
data Family = Family {
      familyId :: FamilyId
    , clients :: ClientMap
    , babiesOnline :: Map BabyName ClientInstance
    }


data Server = Server {
      singles :: Map DeviceId Client -- Clients which are not yet in a family
    , families :: FamilyMap
    , invitations :: Map DeviceId FamilyId
    , babyCount :: Int
    , nextClientId :: TMVar UId
    , nextFamilyId :: TMVar UId
    }

-- Instances: 
instance ToJSON ClientId where
    toJSON (ClientId devId instId) = toJSON $ toUserString devId <> "-" <> tshow instId

instance FromJSON ClientId where
    parseJSON val = do
      (devId', instId') <- T.breakOnEnd "-" <$> parseJSON val
      justZ $ ClientId <$> (stripSuffix "-" devId' >>= fromUserString)
                       <*> readMay instId'

justZ :: MonadPlus m => Maybe a -> m a
justZ = maybe mzero return
        
-- A little Generic vodoo ... 
instance ToJSON ClientServerMessage

instance FromJSON ClientServerMessage
    
instance FromJSON ServerClientMessage

instance ToJSON ServerClientMessage

