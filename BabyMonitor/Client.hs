
module BabyMonitor.Client where

import ClassyPrelude hiding (mapM_)

import Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import BabyMonitor.Types
import qualified Data.Map.Strict as M
import System.Mem.Weak
import Control.Monad.Trans.Maybe
import Control.Monad

make :: Weak WS.Connection -> DeviceId -> (ClientInstance, Client)
make conn did = (newInstance, Client did (M.singleton 0 newInstance) 1)
    where
      newInstance = ClientInstance (ClientId did 0) conn

makeInstance :: Weak WS.Connection -> Client -> (ClientInstance, Client)
makeInstance conn cl = (newInst
                       , cl {
                          instances = M.insert nextId newInst (instances cl)
                        , nextInstanceId = nextId + 1
                        }
                       )
  where
    nextId = nextInstanceId cl
    newInst = ClientInstance (ClientId (deviceId cl) nextId) conn


deleteInstance :: Int -> Client -> Maybe Client
deleteInstance iid cl =
    let
        newInstances = M.delete iid (instances cl)
    in
     if null newInstances then Nothing else Just cl { instances = newInstances }
                                            
send :: ServerClientMessage -> ClientInstance -> IO Bool 
send msg (ClientInstance _ queue) = do
  result <- runMaybeT $ do 
               conn <- MaybeT . deRefWeak $ queue
               MaybeT . ioToMaybe . WS.sendTextData conn . Aeson.encode $ msg
  return $ isJust result
                   
sendBroadcast :: ServerClientMessage -> Client -> IO ()
sendBroadcast msg (Client _ instances _) = mapM_ (send msg) . M.elems $ instances 


                                               
  

ioToMaybe :: IO a -> IO (Maybe a)
ioToMaybe action = (Just <$> action) `catch` exceptionToNothing
    where
      exceptionToNothing :: SomeException -> IO (Maybe a)
      exceptionToNothing _ = return Nothing
