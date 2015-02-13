module BabyPhone.BabyCommunication (
                          BabyConnection
                         , parentSend
                         , parentReceive
                         , babyReceive
                         , babySend
                         , BabyName
                         , Baby
                         , BabyConnections
                         , newBaby
                         , getBabies
                         , popBabyConnection
                         , dropBabyConnection
                         , emptyConnections
                         ) where
                          
import Control.Concurrent.STM.TQueue
import qualified Data.Text as T
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Network.Socket (SockAddr)
import ClassyPrelude

newtype BabyConnection = BabyConnection (
                                         TQueue Text -- baby in
                                        , TQueue Text -- baby out
                                        ) deriving (Eq)

type BabyName = T.Text

type Baby = (BabyName, BabyConnection)
newtype BabyConnections = BabyConnections (M.Map SockAddr ([Baby]))


emptyConnections :: BabyConnections
emptyConnections = BabyConnections M.empty

parentSend :: BabyConnection -> T.Text -> STM ()
parentSend (BabyConnection (babyIn, _)) t = writeTQueue babyIn t

parentReceive :: BabyConnection -> STM T.Text
parentReceive (BabyConnection (_, babyOut)) = readTQueue babyOut

babySend :: BabyConnection -> T.Text -> STM ()
babySend (BabyConnection (_, babyOut)) t = writeTQueue babyOut t

babyReceive :: BabyConnection -> STM T.Text
babyReceive (BabyConnection (babyIn , _)) = readTQueue babyIn


-- Get all the babies registered for a given public IP
getBabies :: BabyConnections -> SockAddr -> [Baby]
getBabies (BabyConnections connections) addr =
    let
        babies = M.lookup addr connections
    in
      case babies of
        Nothing -> []
        Just xs -> xs


addBaby :: BabyConnections -> SockAddr -> Baby -> BabyConnections
addBaby (BabyConnections connections) addr baby = BabyConnections connections'
    where
      connections' =
          M.insertWith -- Replace any old connection
               (\new old -> new ++ filter ((/= name).fst) old)
               addr
               [baby]
               connections
      name = fst baby
  
newBaby :: BabyConnections -> SockAddr -> BabyName -> STM (BabyConnection, BabyConnections)
newBaby connections addr name = do
  connection <- BabyConnection <$> ((,) <$> newTQueue <*> newTQueue)
  return  (connection, addBaby connections addr (name, connection))
 
-- Get rid of a baby, including the addr entry if it was the last baby
-- for this address
popBabyConnection :: BabyConnections -> SockAddr -> BabyName -> (Maybe BabyConnection, BabyConnections)
popBabyConnection bConnections@(BabyConnections connections) addr name =
    (connection, BabyConnections connections')
  where
    babies = getBabies bConnections addr
    connection = lookup name babies
    babies' = filter (((/=) name) . fst) babies
    connections' = updateBabyMap connections addr babies' 


dropBabyConnection :: BabyConnections -> SockAddr -> Baby -> BabyConnections
dropBabyConnection bConnections@(BabyConnections connections) addr baby =
    BabyConnections connections'
    where
      babies' = filter ((/=) baby) (getBabies bConnections addr)
      connections' = updateBabyMap connections addr babies'

updateBabyMap :: M.Map SockAddr ([Baby]) -> SockAddr -> [Baby] -> M.Map SockAddr ([Baby])
updateBabyMap m addr babies = M.update (\_ -> case babies of
                                       [] -> Nothing
                                       _ -> Just babies) addr m