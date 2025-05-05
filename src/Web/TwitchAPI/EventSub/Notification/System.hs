{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.System
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to twitch app status (authorization grants, conduit status)
-}

module Web.TwitchAPI.EventSub.Notification.System where

import Prelude

import Data.Aeson    ( FromJSON(..), (.:)
                     , Object, withObject
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Transport = Transport { method :: Text
                           , sessionID :: Text
                           , connected :: Time.UTCTime
                           , disconnected :: Time.UTCTime
                           }
               deriving ( Show, Eq )
instance FromJSON Transport where
    parseJSON = withObject "Transport" $ \o -> do
        method <- o .: "method"
        sessionID <- o .: "session_id"
        connected <- o .: "connected_at"
        disconnected <- o .: "disconnected_at"
        return Transport{..}

data Message = UserAuthorizationGrant { clientID :: Text
                                      , user :: User
                                      }
             | UserAuthorizationRevoke { clientID :: Text
                                       , user :: User
                                       }
             | UserUpdate { user :: User
                          , email :: Text
                          , verified :: Bool
                          , description :: Text
                          }
             | ConduitShardDisabled { conduitID :: Text
                                    , shardID :: Text
                                    , status :: Text
                                    , transport :: Transport
                                    }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

userAuthorizationGrant :: MessageParser
userAuthorizationGrant o = do
    clientID <- o .: "client_id"
    user <- userFor "" o
    return UserAuthorizationGrant{..}

userAuthorizationRevoke :: MessageParser
userAuthorizationRevoke o = do
    clientID <- o .: "client_id"
    user <- userFor "" o
    return UserAuthorizationRevoke{..}

userUpdate :: MessageParser
userUpdate o = do
    user <- userFor "" o
    email <- o .: "email"
    verified <- o .: "email_verified"
    description <- o .: "description"
    return UserUpdate{..}

conduitShardDisabled :: MessageParser
conduitShardDisabled o = do
    conduitID <- o .: "conduit_id"
    shardID <- o .: "shard_id"
    status <- o .: "status"
    transport <- o .: "transport"
    return ConduitShardDisabled{..}
