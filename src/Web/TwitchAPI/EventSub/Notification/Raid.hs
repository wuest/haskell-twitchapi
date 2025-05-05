{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Raid
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to raids
-}

module Web.TwitchAPI.EventSub.Notification.Raid where

import Prelude

import Data.Aeson    ( (.:), Object )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Message = Raid { origin :: User
                    , destination :: User
                    , viewers :: Integer
                    }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

raid :: MessageParser
raid o = do
    origin <- userFor "from_broadcaster" o
    destination <- userFor "to_broadcaster" o
    viewers <- o .: "viewers"
    return Raid{..}
