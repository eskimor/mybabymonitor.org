module Client where

import ClassyPrelude

import Data.Time.Clock.POSIX
import qualified Data.Map as M

type GroupId = Integer 
type ClientId = Integer 
    
type ClientMap = Map ClientId Client
type GroupMap = Map GroupId Group

type BabyName = Text

data Client = Client {
      clientId :: ClientId
    , babyName :: Maybe Text
    , babyStarted :: Bool
    , toClient :: TQueue Text 
    }

data Group = Group {
      groupId :: GroupId
    , clients :: ClientMap
    , babiesOnline :: Map BabyName Client
    }



data Clients = Clients {
      newClients :: [Clients] -- Clients which are not yet in a group
    , groups :: ClientMap
    , nextClientId :: ClientId
    , nextGroupId :: GroupId
    }


initClients :: IO Clients
initClients = Clients [] M.empty <$> getTime <*> getTime
    where
      getTime = toInteger <$> getPOSIXTime

  
newClient :: STM (Client, Clients)
newClient = undefined
