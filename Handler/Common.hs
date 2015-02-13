-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Network.Socket (SockAddr(..), PortNumber(..))
import BabyPhone.BabyCommunication
import Import
import Text.Julius (rawJS)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


layout :: HeadingPage -> Widget -> Widget
layout pageName pageContent = do
  setTitle "mybabymonitor.org - Web based baby monitor"
  master <- getYesod
  $(widgetFile "layout")

data HeadingPage = Home | Faq | Donate | About | Baby | Parent deriving Eq

headingId :: HeadingPage -> Text
headingId Home = "homeHeading"
headingId Faq = "faqHeading"
headingId Donate = "donateHeading"
headingId About = "aboutHeading"
headingId Baby = "babyHeading"
headingId Parent = "parentHeading"

instance Show HeadingPage where
  show Home = "Home"
  show Faq = "FAQ"
  show Donate = "Donate & Feature Requests"
  show About = "About"
  show Baby  = "Baby"
  show Parent = "Parent"

heading :: HeadingPage -> Widget
heading p = $(widgetFile "heading")
    where
      linkExt Baby = True
      linkExt Parent = True
      linkExt _ = False

      markSelected p' = if p' == p then "class=\"ui-btn-active\"" else ""


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

getClientAddress :: MonadHandler m => m SockAddr
--getClientAddress = filterPort . remoteHost <$> waiRequest
getClientAddress = return $ SockAddrUnix ""



filterPort :: SockAddr -> SockAddr
filterPort (SockAddrInet _ address) = SockAddrInet (PortNum 0) address
filterPort (SockAddrInet6 _ flow address scope) = SockAddrInet6 (PortNum 0) flow address scope
filterPort s = s
