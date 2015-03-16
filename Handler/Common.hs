-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Network.Socket (SockAddr(..), PortNumber(..))
import BabyPhone.BabyCommunication hiding (SockAddr)
import Import
import Text.Julius (rawJS)
import Network.Wai (remoteHost)
import qualified Network.Wai as Wai
import qualified Handler.Session as S

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


--retrieveBabies :: MonadHandler m => m [BabyName]
retrieveBabies :: Handler [BabyName]
retrieveBabies = do
  App {babyConnections = connections} <- getYesod
  address <- getClientAddress
  liftIO . atomically $ fmap fst . flip getBabies address <$> readTVar connections
         
getBabiesR :: Handler TypedContent
getBabiesR = retrieveBabies >>= selectRep . provideRep . returnJson

babyParentCommon :: Text -> Widget
babyParentCommon eventLog = $(widgetFile "common")

getClientAddress :: Handler ByteString
getClientAddress = do
  isReverseProxy <- appIpFromHeader . appSettings <$> getYesod
  if isReverseProxy
    then
        do
          req <- waiRequest
          let headers = Wai.requestHeaders req
          let forwarded = lookup "X-Forwarded-For" headers
          $logDebug $ "Got forwarded IP: " <> tshow forwarded
          case forwarded of
            Nothing -> do
              $logError "Server is configured to use \"X-Forwarded-For\", but none found - fatal. (Security risk!)"
              error "Server is not configured properly, please contact the administrator."
            Just forwarded' -> do
                           deleteSession "ClientIP" -- Should be removed some time
                           return forwarded'
    else
        fromString . show . filterPort . remoteHost <$> waiRequest



filterPort :: SockAddr -> SockAddr
filterPort (SockAddrInet _ address) = SockAddrInet (PortNum 0) address
filterPort (SockAddrInet6 _ flow address scope) = SockAddrInet6 (PortNum 0) flow address scope
filterPort s = s


babiesOnlineText :: BabyCount -> Text
babiesOnlineText 0 = "All babies are awake and with their parents."
babiesOnlineText 1 = "One baby online."
babiesOnlineText n = tshow n <> " babies online."
