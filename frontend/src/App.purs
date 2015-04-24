module App where

import Data.Map 

type State = {
      inFamily :: Boolean
    , babiesOnline :: Map String String
    , babiesCount :: Number
    , serverError :: String
    }
