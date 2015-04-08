module BabyMonitor.Server where

import ClassyPrelude

import BabyMonitor.Types


data Messages =
    InviteClient Text -- stringified id of client to invite
  | DeclineInvitation Text 
