{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.ShieldMode
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions for shield mode status changes
-}

module Web.TwitchAPI.EventSub.Notification.ShieldMode where

import Prelude

import Data.Aeson ( (.:), Object )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Message = Begin { broadcaster :: User
                     , moderator :: User
                     , started :: Time.UTCTime
                     }
             | End { broadcaster :: User
                   , moderator :: User
                   , ended :: Time.UTCTime
                   }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

begin :: MessageParser
begin o = do
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor "moderator" o
    started <- o .: "started_at"
    return Begin{..}

end :: MessageParser
end o = do
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor "moderator" o
    ended <- o .: "ended_at"
    return End{..}
