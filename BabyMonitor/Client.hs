module Client (
               Client
              , make
              ) where

import ClassyPrelude

import Data.Time.Clock.POSIX
import qualified Data.Map as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar


import BabyMonitor.UId
import BabyMonitor.Types

data Messages =
    HandleInvitation Text -- stringified id of invitation sender
  | InvitedClientNotFound Text -- id of not found client
{--
  Messages:
  - Server - Client:
    - handleInvitation: Argument: Inviter id - Just inform the client
      that he got invited to a family. He can then choose to send:
      discardInvation or visit the receiveInvitation page, followed by a
      websocket reconnect.


--}
 
handleInvitation :: Client -> STM ()
handleInvitation (Client _ q) = 
