{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.AdBreak
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to ad breaks
-}

module Web.TwitchAPI.EventSub.Notification.AdBreak where

import Prelude

import Data.Aeson ( (.:), Object )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Message = Begin { duration :: Int
                     , started :: Time.UTCTime
                     , automatic :: Bool
                     , broadcaster :: User
                     , requester :: User
                     }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

begin :: MessageParser
begin o = do
    duration <- o .: "duration_seconds"
    started <- o .: "started_at"
    automatic <- o .: "is_automatic"
    broadcaster <- userFor "broadcaster" o
    requester <- userFor "requester" o
    return Begin{..}
