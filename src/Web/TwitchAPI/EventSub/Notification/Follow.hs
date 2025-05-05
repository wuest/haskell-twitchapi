{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Follow
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to channel follows
-}

module Web.TwitchAPI.EventSub.Notification.Follow where

import Prelude

import Data.Aeson ( (.:), Object )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Message = Follow { user :: User
                      , broadcaster :: User
                      , followedAt :: Time.UTCTime
                      }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

follow :: MessageParser
follow o = do
    user <- userFor "" o
    broadcaster <- userFor "broadcaster" o
    followedAt <- o .: "followed_at"
    return Follow{..}
