{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.VIP
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to VIP management
-}

module Web.TwitchAPI.EventSub.Notification.VIP where

import Prelude

import Data.Aeson    ( Object )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Message = Add { broadcaster :: User
                   , user :: User
                   }
             | Remove { broadcaster :: User
                      , user :: User
                      }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

add :: MessageParser
add o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    return Add{..}

remove :: MessageParser
remove o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    return Remove{..}
