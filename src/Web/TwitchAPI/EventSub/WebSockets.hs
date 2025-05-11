{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

{- |
Module      :  Web.TwitchAPI.EventSub.WebSockets
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable
-}

module Web.TwitchAPI.EventSub.WebSockets where

import Prelude


import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:)
                     , withObject, withText
                     )
import Data.Text     ( Text )

import qualified Data.Time as Time

import qualified Web.TwitchAPI.EventSub.Notification as Notification

-- { "metadata": { "message_id":"2e663ac1-94e5-4bf4-82b6-9076b6b78666"
--               , "message_type":"session_welcome"
--               , "message_timestamp":"2025-05-07T13:55:02.658389334Z"
--               }
--   ,"payload": { "session": { "id":"AgoQA7RX6FefQvSGPVwwSMkawRIGY2VsbC1j"
--                            , "status":"connected"
--                            ,"connected_at":"2025-05-07T13:55:02.654713828Z"
--                            ,"keepalive_timeout_seconds":10
--                            ,"reconnect_url":null
--                            ,"recovery_url":null
--                            }
--               }
-- }

data Metadata = Metadata { messageID :: Text
                         , timestamp :: Time.UTCTime
                         } deriving ( Show, Eq )
instance FromJSON Metadata where
    parseJSON = withObject "metadata" $ \o -> do
        messageID <- o .: "message_id"
        timestamp <- o .: "message_timestamp"
        return Metadata{..}

data Welcome = Welcome { sessionID :: Text
                       , connectedAt :: Time.UTCTime
                       , keepalive :: Integer
                       , reconnectURL :: Maybe Text
                       , recoveryURL :: Maybe Text
                       } deriving ( Show, Eq )
instance FromJSON Welcome where
    parseJSON = withObject "WelcomePayload" $ \o -> do
        session <- o .: "session"
        status :: Text <- session .: "status"
        case status of
            "connected" -> do
                sessionID <- session .: "id"
                connectedAt <- session .: "connected_at"
                keepalive <- session .: "keepalive_timeout_seconds"
                reconnectURL <- session .: "reconnect_url"
                recoveryURL <- session .: "recovery_url"
                return Welcome{..}
            _ -> mzero

data Reconnect = Reconnect { sessionID :: Text
                           , reconnectURL :: Text
                           , connectedAt :: Time.UTCTime
                           } deriving ( Show, Eq )
instance FromJSON Reconnect where
    parseJSON = withObject "ReconnectPayload" $ \o -> do
        session <- o .: "session"
        sessionID <- session .: "id"
        reconnectURL <- session .: "reconnect_url"
        connectedAt <- session .: "connected_at"
        return Reconnect{..}

data RevocationStatus = UserRemoved
                      | AuthorizationRevoked
                      | VersionRemoved
                      deriving ( Show, Eq )
instance FromJSON RevocationStatus where
    parseJSON = withText "RevocationStatus" $ \case
        "user_removed" -> return UserRemoved
        "authorization_revoked" -> return AuthorizationRevoked
        "version_removed" -> return VersionRemoved
        _ -> mzero

-- TODO: Need a type for condition, which would also serve to extract Conditions
-- everywhere else
data Revocation = Revocation { sessionID :: Text
                             , subscriptionID :: Text
                             , status :: RevocationStatus
                             , revocationType :: Text
                             , cost :: Integer
                             } deriving ( Show, Eq )
instance FromJSON Revocation where
    parseJSON = withObject "RevocationPayload" $ \o -> do
        sub <- o .: "subscription"
        transport <- sub .: "transport"
        sessionID <- transport .: "session_id"
        subscriptionID <- sub .: "id"
        status <- sub .: "status"
        revocationType <- sub .: "type"
        cost <- sub .: "cost"
        return Revocation{..}

data Message = WelcomeMessage Metadata Welcome
             | KeepAliveMessage Metadata
             | NotificationMessage Metadata Notification.Notification
             | ReconnectMessage Metadata Reconnect
             | RevocationMessage Metadata Revocation
             deriving ( Show, Eq )
instance FromJSON Message where
    parseJSON = withObject "Message" $ \o -> do
        metadata :: Metadata <- o .: "metadata"
        m <- o .: "metadata"
        mt :: Text <- m .: "message_type"
        case mt of
            "session_welcome" -> do
                payload <- o .: "payload"
                return $ WelcomeMessage metadata payload
            "session_keepalive" -> return $ KeepAliveMessage metadata
            "notification" -> do
                payload <- o .: "payload"
                return $ NotificationMessage metadata payload
            "session_reconnect" -> do
                payload <- o .: "payload"
                return $ ReconnectMessage metadata payload
            "revocation" -> do
                payload <- o .: "payload"
                return $ RevocationMessage metadata payload
            _ -> mzero
