{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.TwitchAPI.Helix.ChannelPoints where

import Prelude

import qualified Data.Time           as Time
import qualified Data.Time.RFC3339   as Time ( formatTimeRFC3339, parseTimeRFC3339 )
import qualified Data.Text           as Text

import Data.Maybe ( fromMaybe )
import Data.Aeson ( FromJSON(..), (.:), (.:?), (.!=), withObject
                  , ToJSON(..), (.=), object
                  , Object, Array
                  )

import qualified Web.TwitchAPI.Helix.Request as Req

createScope :: String
createScope = "channel:manage:redemptions"

createRequest :: (String, String)
createRequest = ("GET", "https://api.twitch.tv/helix/channel_points/custom_rewards")

data Create = Create { broadcasterId :: String
                     } deriving ( Show, Eq )

data CreateJSON = CreateJSON { title :: String
                             , prompt :: Maybe String
                             , cost :: Integer
                             , enabled :: Maybe Bool
                             , backgroundColor :: Maybe String
                             , maxPerStream :: Maybe Integer
                             , maxPerUser :: Maybe Integer
                             , cooldownSeconds :: Maybe Integer
                             , autoFulfilled :: Maybe Bool
                             } deriving ( Show, Eq )

instance ToJSON CreateJSON where
    toJSON CreateJSON{..} =
        object [ "title"      .= (Text.pack title)
               , "prompt"     .= (Text.pack <$> prompt)
               , "cost"       .= cost
               , "is_enabled" .= (fromMaybe True enabled)
               , "background_color" .= (Text.pack <$> backgroundColor)
               , "is_user_input_required" .= (fmap (const True) prompt)
               , "is_max_per_stream_enabled" .= (fmap (const True) maxPerStream)
               , "max_per_stream" .= maxPerStream
               , "is_max_per_user_per_stream_enabled" .= (fmap (const True) maxPerUser)
               , "max_per_user_per_stream" .= maxPerUser
               , "is_global_cooldown_enabled" .= (fmap (const True) cooldownSeconds)
               , "global_cooldown_seconds" .= cooldownSeconds
               , "should_redemptions_skip_request_queue" .= autoFulfilled
               ]
