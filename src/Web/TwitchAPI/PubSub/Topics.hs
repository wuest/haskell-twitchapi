{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.TwitchAPI.PubSub.Topics where

import Prelude

import qualified Data.Time           as Time
import qualified Data.Time.RFC3339   as Time ( formatTimeRFC3339, parseTimeRFC3339 )

import Data.Aeson ( FromJSON(..), (.:), (.:?), withObject, withText, withEmbeddedJSON
                  , ToJSON(..), (.=), object
                  , Object
                  )

import qualified Data.Aeson as JSON

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

-- Used for Channel Points rewards
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

data UserInfo = UserInfo { userId :: Integer
                         , userName :: String
                         , displayName :: Maybe String
                         } deriving ( Eq, Show )

instance FromJSON UserInfo where
    parseJSON = withObject "UserInfo" $ \o -> do
        userId' <- o .: "id"
        let userId = read userId' :: Integer
        userName <- o .: "login"
        displayName <- o .: "display_name"
        return UserInfo{..}

data RewardStatus = Fulfilled | Unfulfilled deriving ( Eq, Show )

instance Read RewardStatus where
    readsPrec _ "FULFILLED" = [(Fulfilled, "")]
    readsPrec _ _ = [(Unfulfilled, "")]


data BadgeUnlock = BadgeUnlock { newVersion :: Integer
                               , previousVersion :: Integer
                               } deriving ( Eq, Show )

instance FromJSON BadgeUnlock where
    parseJSON = withObject "BadgeUnlock" $ \o -> do
        newVersion <- o .: "new_version"
        previousVersion <- o .: "previous_version"
        return BadgeUnlock{..}

data SubscriptionTier = Prime | Tier1 | Tier2 | Tier3 deriving ( Eq, Show )

instance FromJSON SubscriptionTier where
    parseJSON = withText "SubscriptionTier" $ \o ->
        case o of
            "1000" -> return Tier1
            "2000" -> return Tier2
            "3000" -> return Tier3
            "Prime" -> return Prime
            _ -> mzero

data EmoteSpec = EmoteSpec { emoteStart :: Integer
                           , emoteLength :: Integer
                           , emoteId :: Integer
                           } deriving ( Eq, Show )

instance FromJSON EmoteSpec where
    parseJSON = withObject "EmoteSpec" $ \o -> do
        emoteStart <- o .: "start"
        emoteLength <- o .: "end"
        emoteId <- o .: "id"
        return EmoteSpec{..}

data SubscriptionMessage = SubscriptionMessage { messageBody :: String
                                               , emotes :: [EmoteSpec]
                                               } deriving ( Eq, Show )

instance FromJSON SubscriptionMessage where
    parseJSON = withObject "SubscriptionMessage" $ \o -> do
        messageBody <- o .: "message"
        emotes <- o .: "emotes"
        return SubscriptionMessage{..}

data Message = BitsV2Message { badge :: Maybe BadgeUnlock
                             , bits :: Integer
                             , channelId :: Integer
                             , chatMessage :: Maybe String
                             , context :: String
                             , anonymous :: Bool
                             , messageId :: String
                             , messageType :: String
                             , time :: Maybe Time.UTCTime
                             , userTotal :: Integer
                             , userId :: Maybe Integer
                             , userName :: Maybe String
                             , version :: String
                             }
             | BitsV1Message { badge :: Maybe BadgeUnlock
                             , bits :: Integer
                             , channelId :: Integer
                             , channelName :: String
                             , chatMessage :: Maybe String
                             , context :: String
                             , messageId :: String
                             , messageType :: String
                             , time :: Maybe Time.UTCTime
                             , userTotal :: Integer
                             , userId :: Maybe Integer
                             , userName :: Maybe String
                             , version :: String
                             }
             | BitsBadgeMessage { userId :: Maybe Integer
                                , userName :: Maybe String
                                , channelId :: Integer
                                , channelName :: String
                                , bitsTier :: Integer
                                , chatMessage :: Maybe String
                                , time :: Maybe Time.UTCTime
                                }
             | ChannelPointsMessage { serverTime :: Maybe Time.UTCTime
                                    , redeemedTime :: Maybe Time.UTCTime
                                    , user :: UserInfo
                                    , rewardId :: String
                                    , channelId :: Integer
                                    , title :: String
                                    , prompt :: Maybe String
                                    , cost :: Integer
                                    , userInput :: Maybe String
                                    , subOnly :: Bool
                                    , image :: Maybe RewardImages
                                    , defaultImage :: RewardImages
                                    , backgroundColor :: String
                                    , enabled :: Bool
                                    , paused :: Bool
                                    , inStock :: Bool
                                    , maxPerStream :: Maybe Integer
                                    , autoFulfilled :: Bool
                                    , status :: RewardStatus
                                    }
             | ChannelSubscriptionMessage { user :: UserInfo
                                          , channelName :: String
                                          , channelId :: Integer
                                          , time :: Maybe Time.UTCTime
                                          , subTier :: SubscriptionTier
                                          , subPlanName :: String
                                          , subMessage :: SubscriptionMessage
                                          }
             | ChannelResubscriptionMessage { user :: UserInfo
                                            , channelName :: String
                                            , channelId :: Integer
                                            , time :: Maybe Time.UTCTime
                                            , subTier :: SubscriptionTier
                                            , subPlanName :: String
                                            , totalMonths :: Integer
                                            , streakMonths :: Integer
                                            , subMessage :: SubscriptionMessage
                                            }
             | ChannelSubscriptionGiftMessage { user :: UserInfo
                                              , channelName :: String
                                              , channelId :: Integer
                                              , time :: Maybe Time.UTCTime
                                              , subTier :: SubscriptionTier
                                              , subPlanName :: String
                                              , recipient :: UserInfo
                                              }
             | ChannelMultiMonthSubscriptionGiftMessage { user :: UserInfo
                                                        , channelName :: String
                                                        , channelId :: Integer
                                                        , time :: Maybe Time.UTCTime
                                                        , subTier :: SubscriptionTier
                                                        , subPlanName :: String
                                                        , recipient :: UserInfo
                                                        , months :: Integer
                                                        }
             | ChannelAnonymousSubscriptionGiftMessage { channelName :: String
                                                       , channelId :: Integer
                                                       , time :: Maybe Time.UTCTime
                                                       , subTier :: SubscriptionTier
                                                       , subPlanName :: String
                                                       , recipient :: UserInfo
                                                       }
             | ChannelAnonymousMultiMonthSubscriptionGiftMessage { channelName :: String
                                                                 , channelId :: Integer
                                                                 , time :: Maybe Time.UTCTime
                                                                 , subTier :: SubscriptionTier
                                                                 , subPlanName :: String
                                                                 , recipient :: UserInfo
                                                                 , months :: Integer
                                                                 } deriving ( Eq, Show )

parseChannelSubscribeEvent o = do
    uid :: String <- o .: "user_id"
    userName <- o .: "user_name"
    displayName <- o .: "display_name"
    let userId = read uid :: Integer
        user = UserInfo userId userName displayName

    channelName <- o .: "channel_name"
    channel <- o .: "channel_id"
    let channelId = read channel :: Integer

    t :: String <- o .: "time"
    let time = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 t

    subTier <- o .: "sub_plan"
    subPlanName <- o .: "sub_plan_name"
    subMessage <- o .: "sub_message"

    return ChannelSubscriptionMessage{..}

parseChannelResubscribeEvent o = do
    uid :: String <- o .: "user_id"
    userName <- o .: "user_name"
    displayName <- o .: "display_name"
    let userId = read uid :: Integer
        user = UserInfo userId userName displayName

    channelName <- o .: "channel_name"
    channel <- o .: "channel_id"
    let channelId = read channel :: Integer

    t :: String <- o .: "time"
    let time = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 t

    subTier <- o .: "sub_plan"
    subPlanName <- o .: "sub_plan_name"
    totalMonths <- o .: "cumulative_months"
    streakMonths <- o .: "streak_months"
    subMessage <- o .: "sub_message"

    return ChannelResubscriptionMessage{..}

parseChannelSubGiftEvent o = do
    uid :: String <- o .: "user_id"
    userName <- o .: "user_name"
    displayName <- o .: "display_name"
    let userId = read uid :: Integer
        user = UserInfo userId userName displayName

    rid :: String <- o .: "recipient_id"
    rUserName <- o .: "recipient_user_name"
    rDisplayName <- o .: "recipient_display_name"
    let rUserId = read rid :: Integer
        recipient = UserInfo rUserId rUserName rDisplayName

    channelName <- o .: "channel_name"
    channel <- o .: "channel_id"
    let channelId = read channel :: Integer

    t :: String <- o .: "time"
    let time = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 t

    subTier <- o .: "sub_plan"
    subPlanName <- o .: "sub_plan_name"

    duration :: Maybe Integer <- o .: "multi_month_duration"
    case duration of
        Just months -> return ChannelMultiMonthSubscriptionGiftMessage{..}
        _ -> return ChannelSubscriptionGiftMessage{..}

parseChannelAnonSubGiftEvent o = do
    rid :: String <- o .: "recipient_id"
    rUserName <- o .: "recipient_user_name"
    rDisplayName <- o .: "recipient_display_name"
    let rUserId = read rid :: Integer
        recipient = UserInfo rUserId rUserName rDisplayName

    channelName <- o .: "channel_name"
    channel <- o .: "channel_id"
    let channelId = read channel :: Integer

    t :: String <- o .: "time"
    let time = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 t

    subTier <- o .: "sub_plan"
    subPlanName <- o .: "sub_plan_name"

    duration :: Maybe Integer <- o .: "multi_month_duration"
    case duration of
        Just months -> return ChannelAnonymousMultiMonthSubscriptionGiftMessage{..}
        _ -> return ChannelAnonymousSubscriptionGiftMessage{..}

parseChannelSubscribeMessage o = do
    context :: String <- o .: "context"
    case context of
      "sub" -> parseChannelSubscribeEvent o
      "resub" -> parseChannelResubscribeEvent o
      "subgift" -> parseChannelSubGiftEvent o
      "resubgift" -> parseChannelSubGiftEvent o
      "anonsubgift" -> parseChannelAnonSubGiftEvent o
      "anonresubgift" -> parseChannelAnonSubGiftEvent o
      _ -> mzero

parseBitsV2Message o = do
    dat <- o .: "data"
    badge <- dat .: "badge_entitlement"
    bits <- dat .: "bits_used"
    chatMessage <- dat .: "chat_message"
    context <- dat .: "context"
    anonymous <- o .: "is_anonymous"
    messageId <- o .: "message_id" -- Always cheer?  No other examples given in API docs
    messageType <- o .: "message_type" -- Always bits_event?  No other examples given in API docs
    userTotal <- dat .: "total_bits_used"
    userName <- dat .: "user_name"
    version <- o .: "version"

    channel :: String <- dat .: "channel_id"
    let channelId = read channel :: Integer

    uid :: Maybe String <- dat .: "user_id"
    let userId = fmap (read :: String -> Integer) uid

    t :: String <- dat .: "time"
    let time = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 t

    return BitsV2Message{..}

parseBitsV1Message o = do
    dat <- o .: "data"
    badge <- dat .: "badge_entitlement"
    bits <- dat .: "bits_used"
    channelName <- dat .: "channel_name"
    chatMessage <- dat .: "chat_message"
    context <- dat .: "context"
    messageId <- o .: "message_id" -- Always cheer?  No other examples given in API docs
    messageType <- o .: "message_type" -- Always bits_event?  No other examples given in API docs
    userTotal <- dat .: "total_bits_used"
    userName <- dat .: "user_name"
    version <- o .: "version"

    uid :: Maybe String <- dat .: "user_id"
    let userId = fmap (read :: String -> Integer) uid

    channel :: String <- dat .: "channel_id"
    let channelId = read channel :: Integer

    t :: String <- dat .: "time"
    let time = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 t

    return BitsV1Message{..}

parseBitsBadgeMessage o = do
    userId <- o .: "user_id"
    userName <- o .: "user_name"
    channelName <- o .: "channel_name"
    bitsTier <- o .: "badge_tier"
    chatMessage <- o .: "chat_message"

    channel :: String <- o .: "channel_id"
    t :: String <- o .: "time"
    let time = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 t
        channelId = read channel :: Integer
    return BitsBadgeMessage{..}

parseRewardMessage o =  do
    dat <- o .: "data"
    redemption <- dat .: "redemption"
    reward <- redemption .: "reward"
    user <- redemption .: "user"
    rewardId <- reward .: "id"
    title <- reward .: "title"
    prompt <- reward .: "prompt"
    cost <- reward .: "cost"
    subOnly <- reward .: "is_sub_only"
    image <- reward .: "image"
    defaultImage <- reward .: "default_image"
    backgroundColor <- reward .: "background_color"
    enabled <- reward .: "is_enabled"
    paused <- reward .: "is_paused"
    inStock <- reward .: "is_in_stock"
    autoFulfilled <- reward .: "should_redemptions_skip_request_queue"

    channel :: String <- redemption .: "channel_id"
    let channelId = read channel :: Integer

    sTime :: String <- dat .: "timestamp"
    rTime :: String <- redemption .: "redeemed_at"
    let serverTime = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 sTime
        redeemedTime = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 rTime

    promptAnswer <- redemption .:? "user_input"
    promptEnabled :: Bool <- reward .: "is_user_input_required"
    let userInput = if promptEnabled then promptAnswer else Nothing

    maxObject <- reward .: "max_per_stream"
    maxEnabled :: Bool <- maxObject .: "is_enabled"
    maxCount <- maxObject .: "max_per_stream"
    let maxPerStream = if maxEnabled then Just maxCount else Nothing

    status' <- redemption .: "status"
    let status = read status' :: RewardStatus

    return ChannelPointsMessage{..}


instance FromJSON Message where
    parseJSON = withObject "Received" $ \o -> do
        (reportedType :: String) <- o .: "type"
        if reportedType == "MESSAGE"
           then do
               d <- o .: "data"
               m <- d .: "message"
               m' <- withEmbeddedJSON "Message" parseJSON (JSON.String m)
               m' .: "type" >>= \(message :: String) ->
                   case message of
                       "reward-redeemed" -> parseRewardMessage m'
                       _ -> m' .: "topic" >>= \(topic :: String) -> case takeWhile (/= '.') topic of
                           "channel-bits-badge-unlocks" -> parseBitsBadgeMessage m'
                           "channel-bits-event-v1" -> parseBitsV1Message m'
                           "channel-bits-event-v2" -> parseBitsV2Message m'
                           "channel-subscribe-events-v1" -> parseChannelSubscribeMessage m'
                           _ -> mzero
           else mzero

--data ChatModeratorActionsMessage
--data WhispersMessage
