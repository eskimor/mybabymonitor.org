module Handler.Session where
import Import hiding (setSession, lookupSession, getSession, Key)
import qualified Import as I
import qualified Data.Aeson as Aeson



data Key = DeviceId
         | FamilyId deriving Show

setSession :: Aeson.ToJSON a => Key -> a -> Handler ()
setSession k = I.setSessionBS (tshow k)
               . toStrict . Aeson.encode . (:[])


lookupSession :: Aeson.FromJSON a => Key -> Handler (Maybe a)
lookupSession k = do
  mres <- I.lookupSessionBS (tshow k)
  case mres of
    Nothing -> return Nothing
    Just res -> case Aeson.decode . fromStrict $ res of
                     Nothing -> return Nothing
                     Just [a] -> return . Just $ a
                     _ -> error "This should really be a list of values or nothing!"

getSession :: Aeson.FromJSON a => a -> Key -> Handler a
getSession defaultVal k = fromMaybe defaultVal <$> lookupSession k


deleteSession :: Key -> Handler ()
deleteSession k = I.deleteSession (tshow k)
