module Handler.Session where
import qualified Import as I
import Control.Applicative ((<$>))

data Key = BabyName | ClientIP deriving I.Show

setSession :: Key -> I.Text -> I.Handler ()
setSession k v = I.setSession (I.tshow k) v


lookupSession :: Key -> I.Handler (I.Maybe I.Text)
lookupSession k = I.lookupSession (I.tshow k)

getSession :: I.Text -> Key -> I.Handler (I.Text)
getSession defaultVal k = I.fromMaybe defaultVal <$> lookupSession k
