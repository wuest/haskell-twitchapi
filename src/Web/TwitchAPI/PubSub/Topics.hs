{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.TwitchAPI.PubSub.Topics where

import Prelude

import Data.Aeson ( FromJSON(..), (.:), withObject
                  , ToJSON(..), (.=), object
                  , Object
                  )

import Control.Monad ( mzero )

data Topic = BitsV1 { channelId :: Integer }
           | BitsV2 { channelId :: Integer }
           | BitsBadge { channelId :: Integer }
           | ChannelPoints { channelId :: Integer }
           | ChannelSubscriptions { channelId :: Integer }
           | ChatModeratorActions { channelId :: Integer, userId :: Integer }
           | Whispers { userId :: Integer }
           deriving ( Eq, Show )

toRequest :: Topic -> String
toRequest BitsV1{..} = ("channel-bits-events-v1." ++) $ show channelId
toRequest BitsV2{..} = ("channel-bits-events-v2." ++) $ show channelId
toRequest BitsBadge{..} = ("channel-bits-badge-unlocks-v1." ++) $ show channelId
toRequest ChannelPoints{..} = ("channel-points-channel-v1." ++) $ show channelId
toRequest ChannelSubscriptions{..} = ("channel-subscribe-events-v1." ++) $ show channelId
toRequest ChatModeratorActions{..} = "chat_moderator_actions." ++ (show userId) ++ "." ++ (show channelId)
toRequest Whispers{..} = ("whispers." ++) $ show userId

scope :: Topic -> String
scope BitsV1{} = "bits:read"
scope BitsV2{} = "bits:read"
scope BitsBadge{} = "bits:read"
scope ChannelPoints{} = "channel:read:redemptions"
scope ChannelSubscriptions{} = "channel:read:subscriptions"
scope ChatModeratorActions{} = "channel:moderate"
scope Whispers{} = "whispers:read"

data RequestType = Listen | Unlisten deriving ( Eq )

instance Show RequestType where
    show Listen   = "LISTEN"
    show Unlisten = "UNLISTEN"

data Request = Request { requestType :: RequestType
                       , nonce :: Maybe String
                       , topics :: [Topic]
                       , authToken :: String
                       } deriving ( Eq, Show )

instance ToJSON Request where
    toJSON Request{..} =
        object [ "type" .= (show requestType)
               , "nonce" .= nonce
               , "data" .= object [ "topics" .= (toRequest <$> topics)
                                  , "auth_token" .= authToken
                                  ]
               ]

data RequestError = BadMessage | BadAuth | ServerFail | BadTopic | None deriving ( Eq, Show )

instance Read RequestError where
    readsPrec _ "ERR_BADMESSAGE" = [(BadMessage, "")]
    readsPrec _ "ERR_BADAUTH"    = [(BadAuth, "")]
    readsPrec _ "ERR_SERVER"     = [(ServerFail, "")]
    readsPrec _ "ERR_BADTOPIC"   = [(BadTopic, "")]
    readsPrec _ _                = [(None, "")]

data Response = Response { nonce :: Maybe String
                         , errorReported :: RequestError
                         } deriving ( Show, Eq )

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o ->
        o .: "type" >>= \(reportedType :: String) ->
        -- This value is required per Twitch or else any response is invalid
        if reportedType == "RESPONSE" then do
            nonce <- o .: "nonce"
            err <- o .: "error"
            let errorReported = read err :: RequestError
            return Response{..}
        else mzero

data Message = Message { topic :: String
                       , message :: Object
                       } deriving ( Show, Eq )

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o ->
        o .: "type" >>= \(reportedType :: String) ->
            if reportedType == "MESSAGE" then do
                payload <- o .: "data"
                topic <- payload .: "topic"
                message <- payload .: "message"
                return Message{..}
            else mzero

data BitsV2Message = BitsV2Message { badge :: Maybe Object
                                   , bits :: Integer
                                   , channelId :: Integer
                                   , chatMessage :: String
                                   , context :: String
                                   , anonymous :: Bool
                                   , messageId :: String
                                   , messageType :: String
                                   , time :: Time.UTCTime
                                   , userTotal :: Integer
                                   , userId :: Maybe Integer
                                   , userName :: Maybe String
                                   , version :: String
                                   } deriving ( Eq, Show )
