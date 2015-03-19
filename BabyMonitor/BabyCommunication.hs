module BabyMonitor.BabyCommunication (
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
                         , SockAddr
                         , getBabyCount
                         , BabyCount
                         ) where
                          
import Control.Concurrent.STM.TQueue
import qualified Data.Text as T
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import ClassyPrelude

type SockAddr = ByteString
newtype BabyConnection = BabyConnection (
                                         TQueue Text -- baby in
                                        , TQueue Text -- baby out
                                        ) deriving (Eq)

type BabyName = T.Text
type BabyCount = Int

type Baby = (BabyName, BabyConnection)
data BabyConnections = BabyConnections (M.Map SockAddr ([Baby])) BabyCount


emptyConnections :: BabyConnections
emptyConnections = BabyConnections M.empty 0

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
getBabies (BabyConnections connections _) addr =
    let
        babies = M.lookup addr connections
    in
      case babies of
        Nothing -> []
        Just xs -> xs


addBaby :: BabyConnections -> SockAddr -> Baby -> BabyConnections
addBaby (BabyConnections connections babyCount) addr baby = BabyConnections connections' (if babyPresent then babyCount else babyCount + 1)
    where
      babyPresent = case M.lookup addr connections of
                      Nothing -> False
                      Just babies -> any ((== name) . fst) babies
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
popBabyConnection bConnections@(BabyConnections connections count) addr name =
    (connection, BabyConnections connections' (count + babyDiff))
  where
    babies = getBabies bConnections addr
    connection = lookup name babies
    babies' = filter (((/=) name) . fst) babies
    babyDiff = length babies' - length babies
    connections' = updateBabyMap connections addr babies' 


dropBabyConnection :: BabyConnections -> SockAddr -> Baby -> BabyConnections
dropBabyConnection bConnections@(BabyConnections connections count) addr baby =
    BabyConnections connections' (count + babyDiff)
    where
      babies = getBabies bConnections addr
      babies' = filter ((/=) baby) babies
      babyDiff = length babies' - length babies
      connections' = updateBabyMap connections addr babies'

getBabyCount :: BabyConnections -> BabyCount
getBabyCount (BabyConnections _ count) = count

updateBabyMap :: M.Map SockAddr ([Baby]) -> SockAddr -> [Baby] -> M.Map SockAddr ([Baby])
updateBabyMap m addr babies = M.update (\_ -> case babies of
                                       [] -> Nothing
                                       _ -> Just babies) addr m
