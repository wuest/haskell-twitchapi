{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Request
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable
-}

module Web.TwitchAPI.EventSub.Request where

import Prelude


import Data.Aeson ( ToJSON(..), (.=), object
                  , Value( String )
                  )
import Data.Text  ( Text )

import qualified Data.Aeson.Types as JSON.Types

type Callback = Text
type Secret = Text
type SessionID = Text

data Transport = Webhook !Callback !Secret
               | Websocket !SessionID
               deriving ( Show, Eq )
instance ToJSON Transport where
    toJSON (Webhook callback secret) = object [ "method"   .= String "webhook"
                                              , "callback" .= String callback
                                              , "secret"   .= String secret
                                              ]
    toJSON (Websocket sessionID) = object [ "method"     .= String "websocket"
                                          , "session_id" .= String sessionID
                                          ]

data Subscription = AutomodMessageHoldV1 { broadcaster :: Text
                                         , moderator :: Text
                                         }
                  | AutomodMessageHold { broadcaster :: Text
                                       , moderator :: Text
                                       }
                  | AutomodMessageUpdateV1 { broadcaster :: Text
                                           , moderator :: Text
                                           }
                  | AutomodMessageUpdate { broadcaster :: Text
                                         , moderator :: Text
                                         }
                  | AutomodSettingsUpdate { broadcaster :: Text
                                          , moderator :: Text
                                          }
                  | AutomodTermsUpdate { broadcaster :: Text
                                       , moderator :: Text
                                       }
                  | ChannelBitsUse { broadcaster :: Text }
                  | ChannelUpdate { broadcaster :: Text }
                  | ChannelFollow { broadcaster :: Text
                                  , moderator :: Text
                                  }
                  | ChannelAdBreakBegin { broadcaster :: Text }
                  | ChannelChatClear { broadcaster :: Text
                                     , user :: Text
                                     }
                  | ChannelChatClearUserMessages { broadcaster :: Text
                                                 , user :: Text
                                                 }
                  | ChannelChatMessage { broadcaster :: Text
                                       , user :: Text
                                       }
                  | ChannelChatMessageDelete { broadcaster :: Text
                                             , user :: Text
                                             }
                  | ChannelChatNotification { broadcaster :: Text
                                            , user :: Text
                                            }
                  | ChannelChatSettingsUpdate { broadcaster :: Text
                                              , user :: Text
                                              }
                  | ChannelChatUserMessageHold { broadcaster :: Text
                                               , user :: Text
                                               }
                  | ChannelChatUserMessageUpdate { broadcaster :: Text
                                                 , user :: Text
                                                 }
                  | ChannelSharedChatSessionBegin { broadcaster :: Text }
                  | ChannelSharedChatSessionUpdate { broadcaster :: Text }
                  | ChannelSharedChatSessionEnd { broadcaster :: Text }
                  | ChannelSubscribe { broadcaster :: Text }
                  | ChannelSubscriptionEnd { broadcaster :: Text }
                  | ChannelSubscriptionGift { broadcaster :: Text }
                  | ChannelSubscriptionMessage { broadcaster :: Text }
                  | ChannelCheer { broadcaster :: Text }
                  | ChannelRaidFrom { broadcaster :: Text }
                  | ChannelRaidTo { broadcaster :: Text }
                  | ChannelBan { broadcaster :: Text }
                  | ChannelUnban { broadcaster :: Text }
                  | ChannelUnbanRequestCreate { broadcaster :: Text
                                              , moderator :: Text
                                              }
                  | ChannelUnbanRequestResolve { broadcaster :: Text
                                               , moderator :: Text
                                               }
                  | ChannelModerateV1 { broadcaster :: Text
                                      , moderator :: Text
                                      }
                  | ChannelModerate { broadcaster :: Text
                                    , moderator :: Text
                                    }
                  | ChannelModeratorAdd { broadcaster :: Text }
                  | ChannelModeratorRemove { broadcaster :: Text }
                  | ChannelGuestStarSessionBegin { broadcaster :: Text
                                                 , moderator :: Text
                                                 }
                  | ChannelGuestStarSessionEnd { broadcaster :: Text
                                               , moderator :: Text
                                               }
                  | ChannelGuestStarGuestUpdate { broadcaster :: Text
                                                , moderator :: Text
                                                }
                  | ChannelGuestStarSettingsUpdate { broadcaster :: Text
                                                   , moderator :: Text
                                                   }
                  | ChannelPointsAutomaticRewardRedemptionAddV1 { broadcaster :: Text }
                  | ChannelPointsAutomaticRewardRedemptionAdd { broadcaster :: Text }
                  | ChannelPointsCustomRewardAdd { broadcaster :: Text }
                  | ChannelPointsCustomRewardUpdate { broadcaster :: Text
                                                    , rewardID :: Maybe Text
                                                    }
                  | ChannelPointsCustomRewardRemove { broadcaster :: Text
                                                    , rewardID :: Maybe Text
                                                    }
                  | ChannelPointsCustomRewardRedemptionAdd { broadcaster :: Text
                                                           , rewardID :: Maybe Text
                                                           }
                  | ChannelPointsCustomRewardRedemptionUpdate { broadcaster :: Text
                                                              , rewardID :: Maybe Text
                                                              }
                  | ChannelPollBegin { broadcaster :: Text }
                  | ChannelPollProgress { broadcaster :: Text }
                  | ChannelPollEnd { broadcaster :: Text }
                  | ChannelPredictionBegin { broadcaster :: Text }
                  | ChannelPredictionProgress { broadcaster :: Text }
                  | ChannelPredictionLock { broadcaster :: Text }
                  | ChannelPredictionEnd { broadcaster :: Text }
                  | ChannelSuspiciousUserMessage { broadcaster :: Text
                                                 , moderator :: Text
                                                 }
                  | ChannelSuspiciousUserUpdate { broadcaster :: Text
                                                , moderator :: Text
                                                }
                  | ChannelVIPAdd { broadcaster :: Text }
                  | ChannelVIPRemove { broadcaster :: Text }
                  | ChannelWarningAcknowledgement { broadcaster :: Text
                                                  , moderator :: Text
                                                  }
                  | ChannelWarningSend { broadcaster :: Text
                                       , moderator :: Text
                                       }
                  | CharityDonation { broadcaster :: Text }
                  | CharityCampaignStart { broadcaster :: Text }
                  | CharityCampaignProgress { broadcaster :: Text }
                  | CharityCampaignStop { broadcaster :: Text }
                  | ConduitShardDisabled { clientID :: Text
                                         , conduitID :: Maybe Text
                                         }
                  | DropEntitlementGrant { organization :: Text
                                         , category :: Maybe Text
                                         , campaign :: Maybe Text
                                         }
                  | ExtensionBitsTransactionCreate { clientID :: Text }
                  | GoalBegin { broadcaster :: Text }
                  | GoalProgress { broadcaster :: Text }
                  | GoalEnd { broadcaster :: Text }
                  | HypeTrainBegin { broadcaster :: Text }
                  | HypeTrainProgress { broadcaster :: Text }
                  | HypeTrainEnd { broadcaster :: Text }
                  | ShieldModeBegin { broadcaster :: Text
                                    , moderator :: Text
                                    }
                  | ShieldModeEnd { broadcaster :: Text
                                  , moderator :: Text
                                  }
                  | ShoutoutCreate { broadcaster :: Text
                                   , moderator :: Text
                                   }
                  | ShoutoutReceived { broadcaster :: Text
                                     , moderator :: Text
                                     }
                  | StreamOnline { broadcaster :: Text }
                  | StreamOffline { broadcaster :: Text }
                  | UserAuthorizationGrant { clientID :: Text }
                  | UserAuthorizationRevoke { clientID :: Text }
                  | UserUpdate { user :: Text }
                  | WhisperReceived { user :: Text }
                  deriving ( Show, Eq )

access :: Subscription -> [ Text ] -> Bool
access AutomodMessageHoldV1{} xs = "moderator:manage:automod" `elem` xs
access AutomodMessageHold{} xs = "moderator:manage:automod" `elem` xs
access AutomodMessageUpdateV1{} xs = "moderator:manage:automod" `elem` xs
access AutomodMessageUpdate{} xs = "moderator:manage:automod" `elem` xs
access AutomodSettingsUpdate{} xs = "moderator:read:automod_settings" `elem` xs
access AutomodTermsUpdate{} xs = "moderator:manage:automod" `elem` xs
access ChannelBitsUse{} xs = "bits:read" `elem` xs
access ChannelUpdate{} _ = True
access ChannelFollow{} xs = "moderator:read:followers" `elem` xs
access ChannelAdBreakBegin{} xs = "channel:read:ads" `elem` xs
access ChannelChatClear{} xs = all (`elem` xs) [ "user:read:chat", "user:bot", "channel:bot" ]
access ChannelChatClearUserMessages{} xs = all (`elem` xs) [ "user:read:chat", "user:bot", "channel:bot" ]
access ChannelChatMessage{} xs = all (`elem` xs) [ "user:read:chat", "user:bot", "channel:bot" ]
access ChannelChatMessageDelete{} xs = all (`elem` xs) [ "user:read:chat", "user:bot", "channel:bot" ]
access ChannelChatNotification{} xs = all (`elem` xs) [ "user:read:chat", "user:bot", "channel:bot" ]
access ChannelChatSettingsUpdate{} xs = all (`elem` xs) [ "user:read:chat", "user:bot", "channel:bot" ]
access ChannelChatUserMessageHold{} xs = all (`elem` xs) [ "user:read:chat", "user:bot" ]
access ChannelChatUserMessageUpdate{} xs = all (`elem` xs) [ "user:read:chat", "user:bot" ]
access ChannelSharedChatSessionBegin{} _ = True
access ChannelSharedChatSessionUpdate{} _ = True
access ChannelSharedChatSessionEnd{} _ = True
access ChannelSubscribe{} xs = "channel:read:subscriptions" `elem` xs
access ChannelSubscriptionEnd{} xs = "channel:read:subscriptions" `elem` xs
access ChannelSubscriptionGift{} xs = "channel:read:subscriptions" `elem` xs
access ChannelSubscriptionMessage{} xs = "channel:read:subscriptions" `elem` xs
access ChannelCheer{} xs = "bits:read" `elem` xs
access ChannelRaidTo{} _ = True
access ChannelRaidFrom{} _ = True
access ChannelBan{} xs = "channel:moderate" `elem` xs
access ChannelUnban{} xs = "channel:moderate" `elem` xs
access ChannelUnbanRequestCreate{} xs = any (`elem` xs) [ "moderator:read:unban_requests", "moderator:manage:unban_requests" ]
access ChannelUnbanRequestResolve{} xs = any (`elem` xs) [ "moderator:read:unban_requests", "moderator:manage:unban_requests" ]
access ChannelModerateV1{} xs =
    any (`elem` xs) [ "moderator:read:blocked_terms", "moderator:manage:blocked_terms" ]
        && any (`elem` xs) [ "moderator:read:chat_settings", "moderator:manage:chat_settings" ]
        && any (`elem` xs) [ "moderator:read:unban_requests", "moderator:manage:unban_requests" ]
        && any (`elem` xs) [ "moderator:read:banned_users", "moderator:manage:banned_users" ]
        && any (`elem` xs) [ "moderator:read:chat_messages", "moderator:manage:chat_messages" ]
        && all (`elem` xs) [ "moderator:read:moderators", "moderator:read:vips" ]
access ChannelModerate{} xs =
    any (`elem` xs) [ "moderator:read:blocked_terms", "moderator:manage:blocked_terms" ]
        && any (`elem` xs) [ "moderator:read:chat_settings", "moderator:manage:chat_settings" ]
        && any (`elem` xs) [ "moderator:read:unban_requests", "moderator:manage:unban_requests" ]
        && any (`elem` xs) [ "moderator:read:banned_users", "moderator:manage:banned_users" ]
        && any (`elem` xs) [ "moderator:read:chat_messages", "moderator:manage:chat_messages" ]
        && all (`elem` xs) [ "moderator:read:moderators", "moderator:read:vips" ]
access ChannelModeratorAdd{} xs = "moderation:read" `elem` xs
access ChannelModeratorRemove{} xs = "moderation:read" `elem` xs
access ChannelGuestStarSessionBegin{} xs = any (`elem` xs) [ "channel:read:guest_star", "channel:manage:guest_star", "moderator:read:guest_star", "moderator:manage:guest_star" ]
access ChannelGuestStarSessionEnd{} xs = any (`elem` xs) [ "channel:read:guest_star", "channel:manage:guest_star", "moderator:read:guest_star", "moderator:manage:guest_star" ]
access ChannelGuestStarGuestUpdate{} xs = any (`elem` xs) [ "channel:read:guest_star", "channel:manage:guest_star", "moderator:read:guest_star", "moderator:manage:guest_star" ]
access ChannelGuestStarSettingsUpdate{} xs = any (`elem` xs) [ "channel:read:guest_star", "channel:manage:guest_star", "moderator:read:guest_star", "moderator:manage:guest_star" ]
access ChannelPointsAutomaticRewardRedemptionAddV1{} xs = any (`elem` xs) [ "channel:read:redemptions", "channel:manage:redemptions" ]
access ChannelPointsAutomaticRewardRedemptionAdd{} xs = any (`elem` xs) [ "channel:read:redemptions", "channel:manage:redemptions" ]
access ChannelPointsCustomRewardAdd{} xs = any (`elem` xs) [ "channel:read:redemptions", "channel:manage:redemptions" ]
access ChannelPointsCustomRewardUpdate{} xs = any (`elem` xs) [ "channel:read:redemptions", "channel:manage:redemptions" ]
access ChannelPointsCustomRewardRemove{} xs = any (`elem` xs) [ "channel:read:redemptions", "channel:manage:redemptions" ]
access ChannelPointsCustomRewardRedemptionAdd{} xs = any (`elem` xs) [ "channel:read:redemptions", "channel:manage:redemptions" ]
access ChannelPointsCustomRewardRedemptionUpdate{} xs = any (`elem` xs) [ "channel:read:redemptions", "channel:manage:redemptions" ]
access ChannelPollBegin{} xs = any (`elem` xs) [ "channel:read:polls", "channel:manage:polls" ]
access ChannelPollProgress{} xs = any (`elem` xs) [ "channel:read:polls", "channel:manage:polls" ]
access ChannelPollEnd{} xs = any (`elem` xs) [ "channel:read:polls", "channel:manage:polls" ]
access ChannelPredictionBegin{} xs = any (`elem` xs) [ "channel:read:predictions", "channel:manage:predictions" ]
access ChannelPredictionProgress{} xs = any (`elem` xs) [ "channel:read:predictions", "channel:manage:predictions" ]
access ChannelPredictionLock{} xs = any (`elem` xs) [ "channel:read:predictions", "channel:manage:predictions" ]
access ChannelPredictionEnd{} xs = any (`elem` xs) [ "channel:read:predictions", "channel:manage:predictions" ]
access ChannelSuspiciousUserMessage{} xs = "moderator:read:suspicious_users" `elem` xs
access ChannelSuspiciousUserUpdate{} xs = "moderator:read:suspicious_users" `elem` xs
access ChannelVIPAdd{} xs = any (`elem` xs) [ "channel:read:vips", "channel:manage:vips" ]
access ChannelVIPRemove{} xs = any (`elem` xs) [ "channel:read:vips", "channel:manage:vips" ]
access ChannelWarningAcknowledgement{} xs = any (`elem` xs) [ "moderator:read:warnings", "moderator:manage:warnings" ]
access ChannelWarningSend{} xs = any (`elem` xs) [ "moderator:read:warnings", "moderator:manage:warnings" ]
access CharityDonation{} xs = "channel:read:charity" `elem` xs
access CharityCampaignStart{} xs = "channel:read:charity" `elem` xs
access CharityCampaignProgress{} xs = "channel:read:charity" `elem` xs
access CharityCampaignStop{} xs = "channel:read:charity" `elem` xs
access ConduitShardDisabled{} _ = True
access DropEntitlementGrant{} _ = True
access ExtensionBitsTransactionCreate{} _ = True
access GoalBegin{} xs = "channel:read:goals" `elem` xs
access GoalProgress{} xs = "channel:read:goals" `elem` xs
access GoalEnd{} xs = "channel:read:goals" `elem` xs
access HypeTrainBegin{} xs = "channel:read:hype_train" `elem` xs
access HypeTrainProgress{} xs = "channel:read:hype_train" `elem` xs
access HypeTrainEnd{} xs = "channel:read:hype_train" `elem` xs
access ShieldModeBegin{} xs = any (`elem` xs) [ "moderator:read:shield_mode", "moderator:manage:shield_mode" ]
access ShieldModeEnd{} xs = any (`elem` xs) [ "moderator:read:shield_mode", "moderator:manage:shield_mode" ]
access ShoutoutCreate{} xs = any (`elem` xs) [ "moderator:read:shoutouts", "moderator:manage:shoutouts" ]
access ShoutoutReceived{} xs = any (`elem` xs) [ "moderator:read:shoutouts", "moderator:manage:shoutouts" ]
access StreamOnline{} _ = True
access StreamOffline{} _ = True
access UserAuthorizationGrant{} _ = True
access UserAuthorizationRevoke{} _ = True
access UserUpdate{} _ = True
access WhisperReceived{} xs = any (`elem` xs) [ "user:read:whispers", "user:manage:whispers" ]

-- TODO: This is EXTREMELY work in progress; this needs a ton of ergonomic fixes
toRequest :: Transport -> Subscription -> Value
toRequest transport AutomodMessageHoldV1{ broadcaster, moderator } =
    object [ "type"      .= String "automod.message.hold"
           , "version"   .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport AutomodMessageHold{ broadcaster, moderator} =
    object [ "type"    .= String "automod.message.hold"
           , "version" .= String "2"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport AutomodMessageUpdateV1 { broadcaster, moderator } =
    object [ "type"    .= String "automod.message.update"
           , "version" .= String "1"
           , "transport" .= transport
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           ]
toRequest transport AutomodMessageUpdate { broadcaster, moderator } =
    object [ "type"    .= String "automod.message.update"
           , "version" .= String "2"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport AutomodSettingsUpdate { broadcaster, moderator } =
    object [ "type"    .= String "automod.settings.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport AutomodTermsUpdate { broadcaster, moderator } =
    object [ "type"    .= String "automod.terms.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelBitsUse { broadcaster } =
    object [ "type"    .= String "channel.bits.use"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelUpdate { broadcaster } =
    object [ "type"    .= String "channel.update"
           , "version" .= String "2"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelFollow { broadcaster, moderator } =
    object [ "type"    .= String "channel.follow"
           , "version" .= String "2"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelAdBreakBegin { broadcaster } =
    object [ "type"    .= String "channel.ad_break.begin"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatClear { broadcaster, user } =
    object [ "type"    .= String "channel.chat.clear"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatClearUserMessages { broadcaster, user } =
    object [ "type"    .= String "channel.chat.clear_user_messages"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatMessage { broadcaster, user } =
    object [ "type"    .= String "channel.chat.message"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatMessageDelete { broadcaster, user } =
    object [ "type"    .= String "channel.chat.message_delete"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatNotification { broadcaster, user } =
    object [ "type"    .= String "channel.chat.notification"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatSettingsUpdate { broadcaster, user } =
    object [ "type"    .= String "channel.chat_settings.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatUserMessageHold { broadcaster, user } =
    object [ "type"    .= String "channel.chat.user_message_hold"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelChatUserMessageUpdate { broadcaster, user } =
    object [ "type"    .= String "channel.chat.user_message_update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "user_id"             .= String user
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelSubscribe { broadcaster } =
    object [ "type"    .= String "channel.subscribe"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelSubscriptionEnd { broadcaster } =
    object [ "type"    .= String "channel.subscription.end"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelSubscriptionGift { broadcaster } =
    object [ "type"    .= String "channel.subscription.gift"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelSubscriptionMessage { broadcaster } =
    object [ "type"    .= String "channel.subscription.message"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelCheer { broadcaster } =
    object [ "type"    .= String "channel.cheer"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelRaidTo { broadcaster } =
    object [ "type"    .= String "channel.raid"
           , "version" .= String "1"
           , "condition" .= object [ "to_broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelRaidFrom { broadcaster } =
    object [ "type"    .= String "channel.raid"
           , "version" .= String "1"
           , "condition" .= object [ "from_broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelBan { broadcaster } =
    object [ "type"    .= String "channel.ban"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelUnban { broadcaster } =
    object [ "type"    .= String "channel.unban"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelUnbanRequestCreate { broadcaster, moderator } =
    object [ "type"    .= String "channel.unban_request.create"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelUnbanRequestResolve { broadcaster, moderator } =
    object [ "type"    .= String "channel.unban_request.resolve"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelModerateV1 { broadcaster, moderator } =
    object [ "type"    .= String "channel.moderate"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelModerate { broadcaster, moderator } =
    object [ "type"    .= String "channel.moderate"
           , "version" .= String "2"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelModeratorAdd { broadcaster } =
    object [ "type"    .= String "channel.moderator.add"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelModeratorRemove { broadcaster } =
    object [ "type"    .= String "channel.moderator.remove"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelGuestStarSessionBegin { broadcaster, moderator } =
    object [ "type"    .= String "channel.guest_star_session.begin"
           , "version" .= String "beta"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelGuestStarSessionEnd { broadcaster, moderator } =
    object [ "type"    .= String "channel.guest_star_session.end"
           , "version" .= String "beta"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelGuestStarGuestUpdate { broadcaster, moderator } =
    object [ "type"    .= String "channel.guest_star_guest.update"
           , "version" .= String "beta"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelGuestStarSettingsUpdate { broadcaster, moderator } =
    object [ "type"    .= String "channel.guest_star_settings.update"
           , "version" .= String "beta"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsAutomaticRewardRedemptionAddV1 { broadcaster } =
    object [ "type"    .= String "channel.channel_points_automatic_reward_redemption.add"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsAutomaticRewardRedemptionAdd { broadcaster } =
    object [ "type"    .= String "channel.channel_points_automatic_reward_redemption.add"
           , "version" .= String "2"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardAdd { broadcaster } =
    object [ "type"    .= String "channel.channel_points_custom_reward.add"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardUpdate { broadcaster, rewardID=Nothing } =
    object [ "type"    .= String "channel.channel_points_custom_reward.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardUpdate { broadcaster, rewardID=(Just rid) } =
    object [ "type"    .= String "channel.channel_points_custom_reward.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "reward_id"           .= String rid
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardRemove { broadcaster, rewardID=Nothing } =
    object [ "type"    .= String "channel.channel_points_custom_reward.remove"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardRemove { broadcaster, rewardID=(Just rid)} =
    object [ "type"    .= String "channel.channel_points_custom_reward.remove"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "reward_id"           .= String rid
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardRedemptionAdd { broadcaster, rewardID=Nothing } =
    object [ "type"    .= String "channel.channel_points_custom_reward_redemption.add"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardRedemptionAdd { broadcaster, rewardID=(Just rid) } =
    object [ "type"    .= String "channel.channel_points_custom_reward_redemption.add"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "reward_id"           .= String rid
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardRedemptionUpdate { broadcaster, rewardID=Nothing } =
    object [ "type"    .= String "channel.channel_points_custom_reward_redemption.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPointsCustomRewardRedemptionUpdate { broadcaster, rewardID=(Just rid) } =
    object [ "type"    .= String "channel.channel_points_custom_reward_redemption.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "reward_id"           .= String rid
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelPollBegin { broadcaster } =
    object [ "type"    .= String "channel.poll.begin"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPollProgress { broadcaster } =
    object [ "type"    .= String "channel.poll.progress"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPollEnd { broadcaster } =
    object [ "type"    .= String "channel.poll.end"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPredictionBegin { broadcaster } =
    object [ "type"    .= String "channel.prediction.begin"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPredictionProgress { broadcaster } =
    object [ "type"    .= String "channel.prediction.progress"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPredictionLock { broadcaster } =
    object [ "type"    .= String "channel.prediction.lock"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelPredictionEnd { broadcaster } =
    object [ "type"    .= String "channel.prediction.end"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelSuspiciousUserUpdate { broadcaster, moderator } =
    object [ "type"    .= String "channel.suspicious_user.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelSuspiciousUserMessage { broadcaster, moderator } =
    object [ "type"    .= String "channel.suspicious_user.message"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelVIPAdd { broadcaster } =
    object [ "type"    .= String "channel.vip.add"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelVIPRemove { broadcaster } =
    object [ "type"    .= String "channel.vip.remove"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelWarningAcknowledgement { broadcaster, moderator } =
    object [ "type"    .= String "channel.warning.acknowledge"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ChannelWarningSend { broadcaster, moderator } =
    object [ "type"    .= String "channel.warning.send"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport HypeTrainBegin { broadcaster } =
    object [ "type"    .= String "channel.hype_train.begin"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport HypeTrainProgress { broadcaster } =
    object [ "type"    .= String "channel.hype_train.progress"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport HypeTrainEnd { broadcaster } =
    object [ "type"    .= String "channel.hype_train.end"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport CharityDonation { broadcaster } =
    object [ "type"    .= String "channel.charity_campaign.donate"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport CharityCampaignStart { broadcaster } =
    object [ "type"    .= String "channel.charity_campaign.start"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport CharityCampaignProgress { broadcaster } =
    object [ "type"    .= String "channel.charity_campaign.progress"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport CharityCampaignStop { broadcaster } =
    object [ "type"    .= String "channel.charity_campaign.stop"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelSharedChatSessionBegin { broadcaster } =
    object [ "type"    .= String "channel.shared_chat.begin"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelSharedChatSessionUpdate { broadcaster } =
    object [ "type"    .= String "channel.shared_chat.update"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ChannelSharedChatSessionEnd { broadcaster } =
    object [ "type"    .= String "channel.shared_chat.end"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport ShieldModeBegin { broadcaster, moderator } =
    object [ "type"    .= String "channel.shield_mode.begin"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ShieldModeEnd { broadcaster, moderator } =
    object [ "type"    .= String "channel.shield_mode.end"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ShoutoutCreate { broadcaster, moderator } =
    object [ "type"    .= String "channel.shoutout.create"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ShoutoutReceived { broadcaster, moderator } =
    object [ "type"    .= String "channel.shoutout.receive"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster
                                   , "moderator_user_id"   .= String moderator
                                   ]
           , "transport" .= transport
           ]
toRequest transport ConduitShardDisabled { clientID, conduitID=Nothing } =
    object [ "type"    .= String "conduit.shard.disabled"
           , "version" .= String "1"
           , "condition" .= object [ "client_id" .= String clientID ]
           , "transport" .= transport
           ]
toRequest transport ConduitShardDisabled { clientID, conduitID=(Just cid)} =
    object [ "type"    .= String "conduit.shard.disabled"
           , "version" .= String "1"
           , "condition" .= object [ "client_id"  .= String clientID
                                   , "conduit_id" .= String cid
                                   ]
           , "transport" .= transport
           ]
toRequest transport DropEntitlementGrant { organization, category, campaign } =
    let cat :: [JSON.Types.Pair] = maybe [] (\x -> [ "category_id" .= String x ]) category
        cam :: [JSON.Types.Pair] = maybe [] (\x -> [ "campaign_id" .= String x ]) campaign
        con :: [JSON.Types.Pair] = [ "organization_id" .= String organization ] ++ cat ++ cam
    in object [ "type"    .= String "drop.entitlement.grant"
              , "version" .= String "1"
              , "condition" .= object con
              , "transport" .= transport
              ]
toRequest transport ExtensionBitsTransactionCreate { clientID } =
    object [ "type"    .= String "extension.bits_transaction.create"
           , "version" .= String "1"
           , "condition" .= object [ "extension_client_id" .= String clientID ]
           , "transport" .= transport
           ]
toRequest transport GoalBegin { broadcaster } =
    object [ "type"    .= String "channel.goal.begin"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport GoalProgress { broadcaster } =
    object [ "type"    .= String "channel.goal.progress"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport GoalEnd { broadcaster } =
    object [ "type"    .= String "channel.goal.end"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport StreamOnline { broadcaster } =
    object [ "type"    .= String "stream.online"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport StreamOffline { broadcaster } =
    object [ "type"    .= String "stream.offline"
           , "version" .= String "1"
           , "condition" .= object [ "broadcaster_user_id" .= String broadcaster ]
           , "transport" .= transport
           ]
toRequest transport UserAuthorizationGrant { clientID } =
    object [ "type"    .= String "user.authorization.grant"
           , "version" .= String "1"
           , "condition" .= object [ "client_id" .= String clientID ]
           , "transport" .= transport
           ]
toRequest transport UserAuthorizationRevoke { clientID } =
    object [ "type"    .= String "user.authorization.revoke"
           , "version" .= String "1"
           , "condition" .= object [ "client_id" .= String clientID ]
           , "transport" .= transport
           ]
toRequest transport UserUpdate { user } =
    object [ "type"    .= String "user.update"
           , "version" .= String "1"
           , "condition" .= object [ "user_id" .= String user ]
           , "transport" .= transport
           ]
toRequest transport WhisperReceived { user } =
    object [ "type"    .= String "user.whisper.message"
           , "version" .= String "1"
           , "condition" .= object [ "user_id" .= String user ]
           , "transport" .= transport
           ]
