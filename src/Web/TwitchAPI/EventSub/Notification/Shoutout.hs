{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Shoutout
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to shoutouts
-}

module Web.TwitchAPI.EventSub.Notification.Shoutout where

import Prelude

import Data.Aeson ( (.:), Object )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor, userFor' )

data Message = Created { broadcaster :: User
                       , recipient :: User
                       , moderator :: Maybe User
                       , viewers :: Integer
                       , started :: Time.UTCTime
                       , cooldown :: Time.UTCTime
                       , targetCooldown :: Time.UTCTime
                       }
             | Received { broadcaster :: User
                        , sender :: User
                        , viewers :: Integer
                        , started :: Time.UTCTime
                        }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

created :: MessageParser
created o = do
    broadcaster <- userFor "broadcaster" o
    recipient <- userFor "to_broadcaster" o
    moderator <- userFor' "moderator" o
    viewers <- o .: "viewer_count"
    started <- o .: "started_at"
    cooldown <- o .: "cooldown_ends_at"
    targetCooldown <- o .: "target_cooldown_ends_at"
    return Created{..}

received :: MessageParser
received o = do
    broadcaster <- userFor "broadcaster" o
    sender <- userFor "from_broadcaster" o
    viewers <- o .: "viewer_count"
    started <- o .: "started_at"
    return Received{..}
