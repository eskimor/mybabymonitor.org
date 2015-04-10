module BabyMonitor.Types where

import ClassyPrelude
import Data.Aeson
import qualified Data.Text as T

import BabyMonitor.UId

type DeviceId = UId

data ClientId = ClientId {
      devicePart :: DeviceId,
      instancePart :: Int
    } deriving (Show)
              
                
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
