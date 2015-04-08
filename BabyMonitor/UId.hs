module BabyMonitor.UId (make, fromUserString, toUserString, UId) where

-- We are using System.UUID instead of Data.UUID because it generates
-- higher quality uuids. (At least the clock field is randomized)

import System.UUID.V1
import ClassyPrelude
import qualified Codec.Binary.Base32 as B32
import Data.Either.Combinators
import Data.Binary
import qualified Data.Text as T


-- Shortened version of V1 UUID.  We drop the MAC address, as for now
-- the ids are only generated at one host at a time. If this changes
-- it should be pretty easy to add the MAC address to newly created
-- ids without any problems.

-- The reason for shortening is that we want the babymonitor to be as
-- accessible to users as possible. The client ids will be displayed to
-- the user and we don't want to scare them away with unnecessary long
-- ids. 10 bytes is still pretty long and we will use a base 32 code to
-- reduce it to 16 displayed digits, which is manageable.

newtype UId = UId ByteString deriving (Eq, Ord)


-- Convert an uid to a string suitable to be displayed to a user
toUserString :: UId -> Text
toUserString (UId uid) = T.intercalate "-" . T.chunksOf 4
                         . toLower
                         . decodeUtf8 . B32.encode
                         . reverseTimeLow $ uid

fromUserString :: Text -> Maybe UId
fromUserString =  fmap (UId . reverseTimeLow) . rightToMaybe
                  . B32.decode . encodeUtf8
                  . toUpper
                  . filter (/= '-')
         
make :: IO UId
make = UId . toStrict . take 10 . encode <$> uuid
  


-- Reverse the order of the first 4 bytes - the time_low field. We want
-- the fast changing part first, to ensure likely unique values from
-- the first few digits. - The user should, for convenience, only be
-- required to enter a few digits and get an auto complete as soon as
-- the value is unique.
reverseTimeLow :: ByteString -> ByteString
reverseTimeLow bs = (reverse . take 4 $ bs) <> (drop 4 bs)



