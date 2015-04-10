{-# LANGUAGE DeriveGeneric #-}
module BabyMonitor.Server where

import ClassyPrelude
import Data.Aeson


import BabyMonitor.Types


data ClientServerMessage =
    InviteClient DeviceId
  | DeclineInvitation 
  | AnnounceBaby Text
  | RemoveBaby Text
  | GetBabiesOnline
  | MessageToClient ClientId
  | GetAutoComplete Text deriving (Generic, Show)



-- A little Generic vodoo ... 
instance ToJSON ClientServerMessage

instance FromJSON ClientServerMessage
