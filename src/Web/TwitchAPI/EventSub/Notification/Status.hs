{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Status
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to stream status (info update, stream start/end)
-}

module Web.TwitchAPI.EventSub.Notification.Status where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:)
                     , Object, withText
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Category = Category { categoryID :: Text
                         , categoryName :: Text
                         } deriving ( Show, Eq )

data StreamType = Live
                | Playlist
                | WatchParty
                | Premiere
                | Rerun
                deriving ( Show, Eq )
instance FromJSON StreamType where
    parseJSON = withText "StreamType" $ \case
        "live" -> return Live
        "playlist" -> return Playlist
        "watch_party" -> return WatchParty
        "premiere" -> return Premiere
        "rerun" -> return Rerun
        _ -> mzero

data Message = ChannelUpdate { broadcaster :: User
                             , title :: Text
                             , language :: Text
                             , category :: Category
                             , classification :: [Text]
                             }
             | StreamOnline { streamID :: Text
                            , broadcaster :: User
                            , streamType :: StreamType
                            , started :: Time.UTCTime
                            }
             | StreamOffline { broadcaster :: User }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

channelUpdate :: MessageParser
channelUpdate o = do
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    language <- o .: "language"
    categoryID <- o .: "category_id"
    categoryName <- o .: "category_name"
    let category = Category{..}
    classification <- o .: "content_classification_labels"
    return ChannelUpdate{..}

streamOnline :: MessageParser
streamOnline o = do
    streamID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    streamType <- o .: "type"
    started <- o .: "started_at"
    return StreamOnline{..}

streamOffline :: MessageParser
streamOffline o = do
    broadcaster <- userFor "broadcaster" o
    return StreamOffline{..}
