{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.StreamTogether
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to Twitch's multi-user stream functionality (shared chat,
Stream Together)
-}

module Web.TwitchAPI.EventSub.Notification.StreamTogether where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:)
                     , Object, withText
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor, userFor' )

data State = Invited
           | Accepted
           | Ready
           | Backstage
           | Live
           | Removed
           | Empty
           deriving ( Show, Eq )
instance FromJSON State where
    parseJSON = withText "GuestState" $ \case
        "invited" -> return Invited
        "accepted" -> return Accepted
        "ready" -> return Ready
        "backstage" -> return Backstage
        "live" -> return Live
        "removed" -> return Removed
        _ -> mzero

data Layout = Tiled
            | Screenshare
            | HorizontalTop
            | HorizontalBottom
            | VerticalLeft
            | VerticalRight
            deriving ( Show, Eq)
instance FromJSON Layout where
    parseJSON = withText "GuestLayout" $ \case
        "tiled" -> return Tiled
        "screenshare" -> return Screenshare
        "horizontal_top" -> return HorizontalTop
        "horizontal_bottom" -> return HorizontalBottom
        "vertical_left" -> return VerticalLeft
        "vertical_right" -> return VerticalRight
        _ -> mzero

data Share = Share { video :: Bool
                   , audio :: Bool
                   , volume :: Integer
                   } deriving ( Show, Eq )

data Message = SharedChatSessionBegin { sessionID :: Text
                                      , broadcaster :: User
                                      , host :: User
                                      , participants :: [User]
                                      }
             | SharedChatSessionUpdate { sessionID :: Text
                                       , broadcaster :: User
                                       , host :: User
                                       , participants :: [User]
                                       }
             | SharedChatSessionEnd { sessionID :: Text
                                    , broadcaster :: User
                                    , host :: User
                                    }
             | GuestStarSessionBegin { sessionID :: Text
                                     , broadcaster :: User
                                     , started :: Time.UTCTime
                                     }
             | GuestStarSessionEnd { sessionID :: Text
                                   , broadcaster :: User
                                   , host :: User
                                   , started :: Time.UTCTime
                                   , ended :: Time.UTCTime
                                   }
             | GuestStarGuestUpdate { sessionID :: Text
                                    , broadcaster :: User
                                    , moderator :: Maybe User
                                    , guest :: Maybe User
                                    , host :: User
                                    , slotID :: Maybe Text
                                    , state :: State
                                    , hostSharing :: Share
                                    }
             | GuestStarSettingsUpdate { broadcaster :: User
                                       , moderatorControl :: Bool
                                       , slotCount :: Integer
                                       , browserSourceAudioEnabled :: Bool
                                       , layout :: Layout
                                       }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

sharedChatSessionBegin :: MessageParser
sharedChatSessionBegin o = do
    sessionID <- o .: "session_id"
    broadcaster <- userFor "broadcaster" o
    host <- userFor "host_broadcaster" o
    participants' <- o .: "participants"
    participants <- mapM (userFor "broadcaster") participants'
    return SharedChatSessionBegin{..}

sharedChatSessionUpdate :: MessageParser
sharedChatSessionUpdate o = do
    sessionID <- o .: "session_id"
    broadcaster <- userFor "broadcaster" o
    host <- userFor "host_broadcaster" o
    participants' <- o .: "participants"
    participants <- mapM (userFor "broadcaster") participants'
    return SharedChatSessionUpdate{..}

sharedChatSessionEnd :: MessageParser
sharedChatSessionEnd o = do
    sessionID <- o .: "session_id"
    broadcaster <- userFor "broadcaster" o
    host <- userFor "host_broadcaster" o
    return SharedChatSessionEnd{..}

guestStarSessionBegin :: MessageParser
guestStarSessionBegin o = do
    sessionID <- o .: "session_id"
    broadcaster <- userFor "broadcaster" o
    started <- o .: "started_at"
    return GuestStarSessionBegin{..}

guestStarSessionEnd :: MessageParser
guestStarSessionEnd o = do
    sessionID <- o .: "session_id"
    broadcaster <- userFor "broadcaster" o
    host <- userFor "host" o
    started <- o .: "started_at"
    ended <- o .: "ended_at"
    return GuestStarSessionEnd{..}

guestStarGuestUpdate :: MessageParser
guestStarGuestUpdate o = do
    sessionID <- o .: "session_id"
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor' "moderator" o
    guest <- userFor' "guest" o
    host <- userFor "host" o
    slotID <- o .: "slot_id"
    state <- o .: "state"
    video <- o .: "host_video_enabled"
    audio <- o .: "host_audio_enabled"
    volume <- o .: "host_volume"
    let hostSharing = Share{..}
    return GuestStarGuestUpdate{..}

guestStarSettingsUpdate :: MessageParser
guestStarSettingsUpdate o = do
    broadcaster <- userFor "broadcaster" o
    moderatorControl <- o .: "is_moderator_send_live_enabled"
    slotCount <- o .: "slot_count"
    browserSourceAudioEnabled <- o .: "is_browser_source_audio_enabled"
    layout <- o .: "group_layout"
    return GuestStarSettingsUpdate{..}
