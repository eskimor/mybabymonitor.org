{-# LANGUAGE DeriveGeneric #-}

module Client (
               Client
              , make
              ) where

import ClassyPrelude

import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar
import Data.Aeson


import BabyMonitor.UId
import BabyMonitor.Types

data ServerClientMessage =
    HandleInvitation DeviceId 
  | InvitedClientNotFound DeviceId 
  | NotInFamily Text
  | BabiesOnline (M.Map BabyName ClientId)
  | DuplicateBaby Text
  | NotPermitted Text
  | MessageFromClient ClientId Text
  | NoSuchClient ClientId
  | InvalidMessage Text
  | AutoCompleteResult DeviceId deriving (Generic, Show)

instance FromJSON ServerClientMessage

instance ToJSON ServerClientMessage
 
handleInvitation :: Client -> STM ()
handleInvitation (Client _ q) = undefined
