{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.TwitchAPI.Helix.ChannelPoints where

import Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Time             as Time
import qualified Data.Time.RFC3339     as Time ( parseTimeRFC3339 )
import qualified Data.Text             as Text
import qualified Network.HTTP.Client   as HTTP

import Data.Maybe ( fromMaybe )
import Data.Aeson ( FromJSON(..), (.:), withObject
                  , ToJSON(..), (.=), object, encode
                  , Object
                  )

import qualified Web.TwitchAPI.Helix.Request as Req

data Create = Create { broadcasterId :: Integer
                     , title :: String
                     , prompt :: Maybe String
                     , cost :: Integer
                     , enabled :: Maybe Bool
                     , backgroundColor :: Maybe String
                     , maxPerStream :: Maybe Integer
                     , maxPerUser :: Maybe Integer
                     , cooldownSeconds :: Maybe Integer
                     , autoFulfilled :: Maybe Bool
                     } deriving ( Show, Eq )

instance ToJSON Create where
    toJSON Create{..} =
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

instance Req.HelixRequest Create where
    toRequest create =
        let setQuery  = HTTP.setQueryString [("broadcaster_id", Just . BS.pack . show $ (broadcasterId :: Create -> Integer) create)]
            setBody r = r{ HTTP.requestBody = HTTP.RequestBodyLBS . encode . toJSON $ create }
        in setBody . setQuery $ HTTP.parseRequest_ "POST https://api.twitch.tv/helix/channel_points/custom_rewards"
    scope Create{} = Just "channel:manage:redemptions"

data RewardImages = RewardImages { tiny :: Maybe String
                                 , large :: Maybe String
                                 , huge :: Maybe String
                                 } deriving ( Show, Eq )

instance FromJSON RewardImages where
    parseJSON = withObject "RewardImages" $ \o -> do
        tiny <- o .: "url_1x"
        large <- o .: "url_2x"
        huge <- o .: "url_4x"
        return RewardImages{..}

data CreateResponse = CreateResponse { broadcasterId :: Integer
                                     , broadcasterLogin :: String
                                     , broadcasterName :: String
                                     , rewardId :: String
                                     , rewardTitle :: String
                                     , prompt :: Maybe String
                                     , rewardCost :: Integer
                                     , rewardImage :: Maybe RewardImages
                                     , defaultImage :: RewardImages
                                     , backgroundColor :: String
                                     , maxPerStream :: Maybe Integer
                                     , maxPerUser :: Maybe Integer
                                     , cooldownSeconds :: Maybe Integer
                                     , paused :: Bool
                                     , inStock :: Bool
                                     , autoFulfilled :: Bool
                                     , redemptionCount :: Integer
                                     , cooldownExpires :: Maybe Time.UTCTime
                                     } deriving ( Show, Eq )

instance FromJSON CreateResponse where
    parseJSON = withObject "CreateResponse" $ \o -> do
        bid <- o .: "broadcaster_id"
        let broadcasterId = read bid :: Integer
        broadcasterLogin <- o .: "broadcaster_login"
        broadcasterName <- o .: "broadcaster_name"
        rewardId <- o .: "id"
        rewardTitle <- o .: "title"

        promptText <- o .: "prompt"
        promptEnabled :: Bool <- o .: "is_user_input_required"
        let prompt = if promptEnabled then Just promptText else Nothing

        rewardCost <- o .: "cost"
        rewardImage <- o .: "image"
        defaultImage <- o .: "default_image"
        backgroundColor <- o .: "background_color"

        maxObject :: Object <- o .: "max_per_stream_setting"
        maxEnabled <- maxObject .: "is_enabled"
        streamMax <- maxObject .: "max_per_stream"
        let maxPerStream = if maxEnabled then Just streamMax else Nothing

        userMaxObject :: Object <- o .: "max_per_user_per_stream_setting"
        userMaxEnabled <- userMaxObject .: "is_enabled"
        userMax <- userMaxObject .: "max_per_user_per_stream"
        let maxPerUser = if userMaxEnabled then Just userMax else Nothing

        cooldownObject :: Object <- o .: "global_cooldown_setting"
        cooldownEnabled <- cooldownObject .: "is_enabled"
        cooldown <- cooldownObject .: "global_cooldown_seconds"
        let cooldownSeconds = if cooldownEnabled then Just cooldown else Nothing

        paused <- o .: "is_paused"
        inStock <- o .: "is_in_stock"
        autoFulfilled <- o .: "should_redemptions_skip_request_queue"
        redemptionCount <- o .: "redemptions_redeemed_current_stream"
        cooldownExpiry :: Maybe String <- o .: "cooldown_expires_at"
        let cooldownExpires = Time.zonedTimeToUTC <$> (Time.parseTimeRFC3339 =<< cooldownExpiry)
        return CreateResponse{..}
