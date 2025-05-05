{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Chat
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable
-}

module Web.TwitchAPI.EventSub.Notification.Chat where

import Prelude

import Control.Monad ( mzero )
import Data.Maybe    ( fromMaybe )
import Data.Text     ( Text )

import Data.Aeson ( FromJSON(..), (.:), (.:?), Object, withObject, withText )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.Message      ( Badge, MessageType )
import Web.TwitchAPI.EventSub.Notification.Subscription ( SubscriptionTier )
import Web.TwitchAPI.EventSub.Notification.User         ( User, userFor, userFor' )

import qualified Web.TwitchAPI.EventSub.Notification.Currency as Currency
import qualified Web.TwitchAPI.EventSub.Notification.Message  as Message

data ReplyInfo = ReplyInfo { parentID :: Text
                           , parentBody :: Text
                           , replyTo :: User
                           , threadID :: Text
                           , threadUser :: User
                           } deriving ( Show, Eq )
instance FromJSON ReplyInfo where
    parseJSON = withObject "ReplyInfo" $ \o -> do
        parentID <- o .: "parent_message_id"
        parentBody <- o .: "parent_message_body"
        threadID <- o .: "thread_message_id"

        replyTo <- userFor "parent" o
        threadUser <- userFor "thread" o
        return ReplyInfo{..}

type Donation = Currency.Currency

data Status = Approved
            | Denied
            | Invalid
            deriving ( Show, Eq )
instance FromJSON Status where
    parseJSON = withText "Status" $ \case
        "approved" -> return Approved
        "denied" -> return Denied
        "invalid" -> return Invalid
        _ -> mzero

data Notice = Subscription { tier :: SubscriptionTier
                           , duration :: Integer
                           }
            | Resubscription { tier :: SubscriptionTier
                             , cumulativeTotal :: Integer
                             , duration :: Integer
                             , streak :: Integer
                             , gifter :: Maybe User
                             }
            | GiftSubscription { tier :: SubscriptionTier
                               , cumulativeTotal :: Integer
                               , duration :: Integer
                               , recipient :: User
                               , communityGiftID :: Text
                               }
            | CommunityGiftSubscription { communityGiftID :: Text
                                        , count :: Integer
                                        , tier :: SubscriptionTier
                                        , cumulativeTotal :: Integer
                                        }
            | GiftPaidUpgrade { gifter :: Maybe User }
            | PrimePaidUpgrade { tier :: SubscriptionTier }
            | Raid { target :: User
                   , count :: Integer
                   , profileImageURL :: Text
                   }
            | Unraid
            | PayItForward { gifter :: Maybe User }
            | Announcement { decorationColor :: Text }
            | BitsBadgeTier { bitsTier :: Integer }
            | CharityDonation { charity :: Text
                              , donationAmount :: Donation
                              }
            deriving ( Show, Eq )
instance FromJSON Notice where
    parseJSON = withObject "Notice" $ \o -> do
        noticeType :: Text <- o .: "notice_type"
        case noticeType of
            "sub" -> do
                sub <- o .: "sub"
                tier <- o .: "sub"
                duration <- sub .: "duration"
                return Subscription{..}
            "resub" -> do
                resub <- o .: "resub"
                tier <- resub .: "sub_tier"
                duration <- resub .: "duration_months"
                streak <- resub .: "streak_months"
                cumulativeTotal <- resub .: "cumulative_months"
                anon <- resub .: "gifter_is_anonymous"
                gifter <- if anon then return Nothing
                                  else userFor' "gifter" resub
                return Resubscription{..}
            "sub_gift" -> do
                sg <- o .: "sub_gift"
                tier <- sg .: "sub_tier"
                ct <- sg .:? "cumulative_total"
                let cumulativeTotal = case ct of
                                            (Just t) -> t
                                            _ -> 0
                duration <- sg .: "duration_months"
                recipient <- userFor "recipient" sg
                communityGiftID <- sg .: "community_gift_id"
                return GiftSubscription{..}
            "community_sub_gift" -> do
                cgs <- o .: "community_sub_gift"
                communityGiftID <- cgs .: "id"
                count <- cgs .: "total"
                tier <- cgs .: "sub_tier"
                ct <- cgs .:? "cumulative_total"
                let cumulativeTotal = case ct of
                                            (Just t) -> t
                                            _ -> 0
                return CommunityGiftSubscription{..}
            "gift_paid_upgrade" -> do
                gpu <- o .: "prime_paid_upgrade"
                anon <- gpu .: "gifter_is_anonymous"
                gifter <- if anon then return Nothing
                                  else userFor' "gifter" gpu
                return GiftPaidUpgrade{..}
            "prime_paid_upgrade" -> do
                ppu <- o .: "prime_paid_upgrade"
                tier <- ppu .: "sub_tier"
                return PrimePaidUpgrade{..}
            "raid" -> do
                raid <- o .: "raid"
                target <- userFor "" raid

                count <- raid .: "viewer_count"
                profileImageURL <- raid .: "profile_image_url"
                return Raid{..}
            "unraid" ->
                return Unraid
            "pay_it_forward" -> do
                pif <- o .: "pay_it_forward"
                anon <- pif .: "user_is_anonymous"
                gifter <- if anon then return Nothing
                                  else userFor' "gifter" pif
                return PayItForward{..}
            "announcement" -> do
                ann <- o .: "announcement"
                decorationColor <- ann .: "color"
                return Announcement{..}
            "bits_badge_tier" -> do
                bt <- o .: "bits_badge_tier"
                bitsTier <- bt .: "tier"
                return BitsBadgeTier{..}
            "charity_donation" -> do
                cd <- o .: "charity_donation"
                charity <- cd .: "charity"
                donationAmount <- cd .: "amount"
                return CharityDonation{..}
            "shared_chat_sub" -> do
                sub <- o .: "sub"
                tier <- o .: "sub"
                duration <- sub .: "duration"
                return Subscription{..}
            "shared_chat_resub" -> do
                resub <- o .: "resub"
                tier <- resub .: "sub_tier"
                duration <- resub .: "duration_months"
                streak <- resub .: "streak_months"
                cumulativeTotal <- resub .: "cumulative_months"
                anon <- resub .: "gifter_is_anonymous"
                gifter <- if anon then return Nothing
                                  else userFor' "gifter" resub
                return Resubscription{..}
            "shared_chat_sub_gift" -> do
                sg <- o .: "sub_gift"
                tier <- sg .: "sub_tier"
                ct <- sg .:? "cumulative_total"
                let cumulativeTotal = case ct of
                                            (Just t) -> t
                                            _ -> 0
                duration <- sg .: "duration_months"
                recipient <- userFor "recipient" sg
                communityGiftID <- sg .: "community_gift_id"
                return GiftSubscription{..}
            "shared_chat_community_sub_gift" -> do
                cgs <- o .: "community_sub_gift"
                communityGiftID <- cgs .: "id"
                count <- cgs .: "total"
                tier <- cgs .: "sub_tier"
                ct <- cgs .:? "cumulative_total"
                let cumulativeTotal = case ct of
                                            (Just t) -> t
                                            _ -> 0
                return CommunityGiftSubscription{..}
            "shared_chat_gift_paid_upgrade" -> do
                gpu <- o .: "prime_paid_upgrade"
                anon <- gpu .: "gifter_is_anonymous"
                gifter <- if anon then return Nothing
                                  else userFor' "gifter" gpu
                return GiftPaidUpgrade{..}
            "shared_chat_prime_paid_upgrade" -> do
                ppu <- o .: "prime_paid_upgrade"
                tier <- ppu .: "sub_tier"
                return PrimePaidUpgrade{..}
            "shared_chat_raid" -> do
                raid <- o .: "raid"
                target <- userFor "" raid

                count <- raid .: "viewer_count"
                profileImageURL <- raid .: "profile_image_url"
                return Raid{..}
            "shared_chat_pay_it_forward" -> do
                pif <- o .: "pay_it_forward"
                anon <- pif .: "user_is_anonymous"
                gifter <- if anon then return Nothing
                                  else userFor' "gifter" pif
                return PayItForward{..}
            "shared_chat_announcement" -> do
                ann <- o .: "announcement"
                decorationColor <- ann .: "color"
                return Announcement{..}
            _ -> mzero

data Message = Clear { broadcaster :: User }
             | ClearUserMessages { broadcaster :: User
                                 , user :: User
                                 }
             | Message { broadcaster :: User
                       , user :: User
                       , message :: Message.Message
                       , messageType :: MessageType
                       , badges :: [Badge]
                       , bits :: Integer
                       , color :: Maybe Text
                       , reply :: Maybe ReplyInfo
                       , channelPointsReward :: Maybe Text
                       , sourceBroadcaster :: Maybe User
                       , sourceBadges :: [Badge]
                       , sourceOnly :: Bool
                       }
             | MessageDelete { broadcaster :: User
                             , user :: User
                             , messageID :: Text
                             }
             | Notification { broadcaster :: User
                            , user :: User
                            , anonymous :: Bool
                            , color :: Maybe Text
                            , badges :: [Badge]
                            , systemMessage :: Text
                            , message :: Message.Message
                            , notice :: Notice
                            , sourceBroadcaster :: Maybe User
                            , sourceBadges :: [Badge]
                            , sourceMessageID :: Maybe Text
                            }
             | SettingsUpdate { broadcaster :: User
                              , emoteMode :: Bool
                              , followerMode :: Maybe Integer
                              , slowMode :: Maybe Integer
                              , subscriberMode :: Bool
                              , uniqueChatMode :: Bool
                              }
             | UserMessageHold { broadcaster :: User
                               , user :: User
                               , message :: Message.Message
                               }
             | UserMessageUpdate { broadcaster :: User
                                 , user :: User
                                 , status :: Status
                                 , message :: Message.Message
                                 }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

chatClear :: MessageParser
chatClear o = do
    broadcaster <- userFor "broadcaster" o
    return Clear{..}

clearUserMessages :: MessageParser
clearUserMessages o = do
    e <- o .: "event"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "target" e
    return ClearUserMessages{..}

chatMessage :: MessageParser
chatMessage o = do
    e <- o .: "event"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "chatter" e

    message <- o .: "event"
    messageType <- e .: "message_type"
    badges <- e .: "badges"
    cheer <- e .: "cheer"
    bits <- case cheer of
                (Just b) -> b .: "bits"
                Nothing -> return 0
    color <- e .:? "color"
    reply <- e .:? "reply"
    channelPointsReward <- e .:? "channel_points_custom_reward_id"
    sourceBroadcaster <- userFor' "source_broadcaster" e
    sourceBadges <- e .: "source_badges"
    sourceOnly' <- e .: "source_only"

    let sourceOnly = fromMaybe False sourceOnly'
    return Message{..}

chatMessageDelete :: MessageParser
chatMessageDelete o = do
    e <- o .: "event"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "target" e
    messageID <- e .: "message_id"
    return MessageDelete{..}

chatNotification :: MessageParser
chatNotification o = do
    e <- o .: "event"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "chatter" e

    anonymous <- e .: "chatter_is_anonymous"
    color <- e .:? "color"
    badges <- e .: "badges"
    systemMessage <- e .: "system_message"
    message <- o .: "event"
    notice <- o .: "event"

    sourceBroadcaster <- userFor' "source_broadcaster" e
    sourceBadges <- e .: "source_badges"
    sourceMessageID <- e .:? "source_message_id"
    return Notification{..}

chatSettingsUpdate :: MessageParser
chatSettingsUpdate o = do
    broadcaster <- userFor "broadcaster" o
    e <- o .: "event"
    emoteMode <- e .: "emote_mode"
    followerMode <- e .:? "follower_mode_duration_minutes"
    slowMode <- e .:? "slow_mode_wait_time_seconds"
    subscriberMode <- e .: "subscriber_mode"
    uniqueChatMode <- e .: "unique_chat_mode"
    return SettingsUpdate{..}

userMessageHold :: MessageParser
userMessageHold o = do
    e <- o .: "event"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" e
    message <- o .: "event"
    return UserMessageHold{..}

userMessageUpdate :: MessageParser
userMessageUpdate o = do
    e <- o .: "event"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" e

    status <- e .: "status"
    message <- o .: "event"
    return UserMessageUpdate{..}
