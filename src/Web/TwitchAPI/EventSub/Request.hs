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

import Control.Monad ( mzero )
import Data.Text     ( Text )

import Data.Aeson ( FromJSON(..), (.:), withObject
                  , ToJSON(..), (.=), object
                  , Value( String )
                  )

data Subscription = AutomodMessageHoldV1
                  | AutomodMessageHold
                  | AutomodMessageUpdateV1
                  | AutomodMessageUpdate
                  | AutomodSettingsUpdate
                  | AutomodTermsUpdate
                  | ChannelBitsUse
                  | ChannelUpdate
                  | ChannelFollow
                  | ChannelAdBreakBegin
                  | ChannelChatClear
                  | ChannelChatClearUserMessages
                  | ChannelChatMessage
                  | ChannelChatMessageDelete
                  | ChannelChatNotification
                  | ChannelChatSettingsUpdate
                  | ChannelChatUserMessageHold
                  | ChannelChatUserMessageUpdate
                  | ChannelSharedChatSessionBegin
                  | ChannelSharedChatSessionUpdate
                  | ChannelSharedChatSessionEnd
                  | ChannelSubscribe
                  | ChannelSubscriptionEnd
                  | ChannelSubscriptionGift
                  | ChannelSubscriptionMessage
                  | ChannelCheer
                  | ChannelRaid
                  | ChannelBan
                  | ChannelUnban
                  | ChannelUnbanRequestCreate
                  | ChannelUnbanRequestResolve
                  | ChannelModerateV1
                  | ChannelModerate
                  | ChannelModeratorAdd
                  | ChannelModeratorRemove
                  | ChannelGuestStarSessionBegin
                  | ChannelGuestStarSessionEnd
                  | ChannelGuestStarGuestUpdate
                  | ChannelGuestStarSettingsUpdate
                  | ChannelPointsAutomaticRewardRedemptionAddV1
                  | ChannelPointsAutomaticRewardRedemptionAdd
                  | ChannelPointsCustomRewardAdd
                  | ChannelPointsCustomRewardUpdate
                  | ChannelPointsCustomRewardRemove
                  | ChannelPointsCustomRewardRedemptionAdd
                  | ChannelPointsCustomRewardRedemptionUpdate
                  | ChannelPollBegin
                  | ChannelPollProgress
                  | ChannelPollEnd
                  | ChannelPredictionBegin
                  | ChannelPredictionProgress
                  | ChannelPredictionLock
                  | ChannelPredictionEnd
                  | ChannelSuspiciousUserMessage
                  | ChannelSuspiciousUserUpdate
                  | ChannelVIPAdd
                  | ChannelVIPRemove
                  | ChannelWarningAcknowledgement
                  | ChannelWarningSend
                  | CharityDonation
                  | CharityCampaignStart
                  | CharityCampaignProgress
                  | CharityCampaignStop
                  | ConduitShardDisabled
                  | DropEntitlementGrant
                  | ExtensionBitsTransactionCreate
                  | GoalBegin
                  | GoalProgress
                  | GoalEnd
                  | HypeTrainBegin
                  | HypeTrainProgress
                  | HypeTrainEnd
                  | ShieldModeBegin
                  | ShieldModeEnd
                  | ShoutoutCreate
                  | ShoutoutReceived
                  | StreamOnline
                  | StreamOffline
                  | UserAuthorizationGrant
                  | UserAuthorizationRevoke
                  | UserUpdate
                  | WhisperReceived
                  deriving ( Show, Eq )

-- Scopes related to each subscription type
-- If more than one scope is listed, often there are only a subset required
-- This API should be considered unstable.
scopes :: Subscription -> [ Text ]
scopes AutomodMessageHoldV1 = [ "moderator:manage:automod" ]
scopes AutomodMessageHold = [ "moderator:manage:automod" ]
scopes AutomodMessageUpdateV1 = [ "moderator:manage:automod" ]
scopes AutomodMessageUpdate = [ "moderator:manage:automod" ]
scopes AutomodSettingsUpdate = [ "moderator:read:automod_settings" ]
scopes AutomodTermsUpdate = [ "moderator:manage:automod" ]
scopes ChannelBitsUse = [ "bits:read" ]
scopes ChannelUpdate = []
scopes ChannelFollow = [ "moderator:read:followers" ]
scopes ChannelAdBreakBegin = [ "channel:read:ads" ]
scopes ChannelChatClear = [ "user:read:chat", "user:bot", "channel:bot" ]
scopes ChannelChatClearUserMessages = [ "user:read:chat", "user:bot", "channel:bot" ]
scopes ChannelChatMessage = [ "user:read:chat", "user:bot", "channel:bot" ]
scopes ChannelChatMessageDelete = [ "user:read:chat", "user:bot", "channel:bot" ]
scopes ChannelChatNotification = [ "user:read:chat", "user:bot", "channel:bot" ]
scopes ChannelChatSettingsUpdate = [ "user:read:chat", "user:bot", "channel:bot" ]
scopes ChannelChatUserMessageHold = [ "user:read:chat", "user:bot" ]
scopes ChannelChatUserMessageUpdate = [ "user:read:chat", "user:bot" ]
scopes ChannelSharedChatSessionBegin = []
scopes ChannelSharedChatSessionUpdate = []
scopes ChannelSharedChatSessionEnd = []
scopes ChannelSubscribe = [ "channel:read:subscriptions" ]
scopes ChannelSubscriptionEnd = [ "channel:read:subscriptions" ]
scopes ChannelSubscriptionGift = [ "channel:read:subscriptions" ]
scopes ChannelSubscriptionMessage = [ "channel:read:subscriptions" ]
scopes ChannelCheer = [ "bits:read" ]
scopes ChannelRaid = []
scopes ChannelBan = [ "channel:moderate" ]
scopes ChannelUnban = [ "channel:moderate" ]
scopes ChannelUnbanRequestCreate = [ "moderator:read:unban_requests", "moderator:manage:unban_requests" ]
scopes ChannelUnbanRequestResolve = [ "moderator:read:unban_requests", "moderator:manage:unban_requests" ]
scopes ChannelModerateV1 = [ "moderator:read:blocked_terms"
                           , "moderator:manage:blocked_terms"
                           , "moderator:read:chat_settings"
                           , "moderator:manage:chat_settings"
                           , "moderator:read:unban_requests"
                           , "moderator:manage:unban_requests"
                           , "moderator:read:banned_users"
                           , "moderator:manage:banned_users"
                           , "moderator:read:chat_messages"
                           , "moderator:manage:chat_messages"
                           , "moderator:read:moderators"
                           , "moderator:read:vips"
                           ]
scopes ChannelModerate = [ "moderator:read:blocked_terms"
                         , "moderator:manage:blocked_terms"
                         , "moderator:read:chat_settings"
                         , "moderator:manage:chat_settings"
                         , "moderator:read:unban_requests"
                         , "moderator:manage:unban_requests"
                         , "moderator:read:banned_users"
                         , "moderator:manage:banned_users"
                         , "moderator:read:chat_messages"
                         , "moderator:manage:chat_messages"
                         , "moderator:read:moderators"
                         , "moderator:read:vips"
                         ]
scopes ChannelModeratorAdd = [ "moderation:read" ]
scopes ChannelModeratorRemove = [ "moderation:read" ]
scopes ChannelGuestStarSessionBegin = [ "channel:read:guest_star"
                                      , "channel:manage:guest_star"
                                      , "moderator:read:guest_star"
                                      , "moderator:manage:guest_star"
                                      ]
scopes ChannelGuestStarSessionEnd = [ "channel:read:guest_star"
                                    , "channel:manage:guest_star"
                                    , "moderator:read:guest_star"
                                    , "moderator:manage:guest_star"
                                    ]
scopes ChannelGuestStarGuestUpdate = [ "channel:read:guest_star"
                                     , "channel:manage:guest_star"
                                     , "moderator:read:guest_star"
                                     , "moderator:manage:guest_star"
                                     ]
scopes ChannelGuestStarSettingsUpdate = [ "channel:read:guest_star"
                                        , "channel:manage:guest_star"
                                        , "moderator:read:guest_star"
                                        , "moderator:manage:guest_star"
                                        ]
scopes ChannelPointsAutomaticRewardRedemptionAddV1 = [ "channel:read:redemptions", "channel:manage:redemptions" ]
scopes ChannelPointsAutomaticRewardRedemptionAdd = [ "channel:read:redemptions", "channel:manage:redemptions" ]
scopes ChannelPointsCustomRewardAdd = [ "channel:read:redemptions", "channel:manage:redemptions" ]
scopes ChannelPointsCustomRewardUpdate = [ "channel:read:redemptions", "channel:manage:redemptions" ]
scopes ChannelPointsCustomRewardRemove = [ "channel:read:redemptions", "channel:manage:redemptions" ]
scopes ChannelPointsCustomRewardRedemptionAdd = [ "channel:read:redemptions", "channel:manage:redemptions" ]
scopes ChannelPointsCustomRewardRedemptionUpdate = [ "channel:read:redemptions", "channel:manage:redemptions" ]
scopes ChannelPollBegin = [ "channel:read:polls", "channel:manage:polls" ]
scopes ChannelPollProgress = [ "channel:read:polls", "channel:manage:polls" ]
scopes ChannelPollEnd = [ "channel:read:polls", "channel:manage:polls" ]
scopes ChannelPredictionBegin = [ "channel:read:predictions", "channel:manage:predictions" ]
scopes ChannelPredictionProgress = [ "channel:read:predictions", "channel:manage:predictions" ]
scopes ChannelPredictionLock = [ "channel:read:predictions", "channel:manage:predictions" ]
scopes ChannelPredictionEnd = [ "channel:read:predictions", "channel:manage:predictions" ]
scopes ChannelSuspiciousUserMessage = [ "moderator:read:suspicious_users" ]
scopes ChannelSuspiciousUserUpdate = [ "moderator:read:suspicious_users" ]
scopes ChannelVIPAdd = [ "channel:read:vips", "channel:manage:vips" ]
scopes ChannelVIPRemove = [ "channel:read:vips", "channel:manage:vips" ]
scopes ChannelWarningAcknowledgement = [ "moderator:read:warnings", "moderator:manage:warnings" ]
scopes ChannelWarningSend = [ "moderator:read:warnings", "moderator:manage:warnings" ]
scopes CharityDonation = [ "channel:read:charity" ]
scopes CharityCampaignStart = [ "channel:read:charity" ]
scopes CharityCampaignProgress = [ "channel:read:charity" ]
scopes CharityCampaignStop = [ "channel:read:charity" ]
scopes ConduitShardDisabled = []
scopes DropEntitlementGrant = []
scopes ExtensionBitsTransactionCreate = []
scopes GoalBegin = [ "channel:read:goals" ]
scopes GoalProgress = [ "channel:read:goals" ]
scopes GoalEnd = [ "channel:read:goals" ]
scopes HypeTrainBegin = [ "channel:read:hype_train" ]
scopes HypeTrainProgress = [ "channel:read:hype_train" ]
scopes HypeTrainEnd = [ "channel:read:hype_train" ]
scopes ShieldModeBegin = [ "moderator:read:shield_mode", "moderator:manage:shield_mode" ]
scopes ShieldModeEnd = [ "moderator:read:shield_mode", "moderator:manage:shield_mode" ]
scopes ShoutoutCreate = [ "moderator:read:shoutouts", "moderator:manage:shoutouts" ]
scopes ShoutoutReceived = [ "moderator:read:shoutouts", "moderator:manage:shoutouts" ]
scopes StreamOnline = []
scopes StreamOffline = []
scopes UserAuthorizationGrant = []
scopes UserAuthorizationRevoke = []
scopes UserUpdate = [ "user:read:email" ]
scopes WhisperReceived = [ "user:read:whispers", "user:manage:whispers" ]

instance FromJSON Subscription where
    parseJSON = withObject "subscription" $ \o -> do
        subType :: Text <- o .: "type"
        version :: Text <- o .: "version"
        case (subType, version) of
            ("automod.message.hold", "1") -> return AutomodMessageHoldV1
            ("automod.message.hold", "2") -> return AutomodMessageHold
            ("automod.message.update", "1") -> return AutomodMessageUpdateV1
            ("automod.message.update", "2") -> return AutomodMessageUpdate
            ("automod.settings.update", "1") -> return AutomodSettingsUpdate
            ("automod.terms.update", "1") -> return AutomodTermsUpdate
            ("channel.bits.use", "1") -> return ChannelBitsUse
            ("channel.update", "2") -> return ChannelUpdate
            ("channel.follow", "2") -> return ChannelFollow
            ("channel.ad_break.begin", "1") -> return ChannelAdBreakBegin
            ("channel.chat.clear", "1") -> return ChannelChatClear
            ("channel.chat.clear_user_messages", "1") -> return ChannelChatClearUserMessages
            ("channel.chat.message", "1") -> return ChannelChatMessage
            ("channel.chat.message_delete", "1") -> return ChannelChatMessageDelete
            ("channel.chat.notification", "1") -> return ChannelChatNotification
            ("channel.chat_settings.update", "1") -> return ChannelChatSettingsUpdate
            ("channel.chat.user_message_hold", "1") -> return ChannelChatUserMessageHold
            ("channel.chat.user_message_update", "1") -> return ChannelChatUserMessageUpdate
            ("channel.shared_chat.begin", "1") -> return ChannelSharedChatSessionBegin
            ("channel.shared_chat.update", "1") -> return ChannelSharedChatSessionUpdate
            ("channel.shared_chat.end", "1") -> return ChannelSharedChatSessionEnd
            ("channel.subscribe", "1") -> return ChannelSubscribe
            ("channel.subscription.end", "1") -> return ChannelSubscriptionEnd
            ("channel.subscription.gift", "1") -> return ChannelSubscriptionGift
            ("channel.subscription.message", "1") -> return ChannelSubscriptionMessage
            ("channel.cheer", "1") -> return ChannelCheer
            ("channel.raid", "1") -> return ChannelRaid
            ("channel.ban", "1") -> return ChannelBan
            ("channel.unban", "1") -> return ChannelUnban
            ("channel.unban_request.create", "1") -> return ChannelUnbanRequestCreate
            ("channel.unban_request.resolve", "1") -> return ChannelUnbanRequestResolve
            ("channel.moderate", "1") -> return ChannelModerateV1
            ("channel.moderate", "2") -> return ChannelModerate
            ("channel.moderator.add", "1") -> return ChannelModeratorAdd
            ("channel.moderator.remove", "1") -> return ChannelModeratorRemove
            ("channel.guest_star_session.begin", "beta") -> return ChannelGuestStarSessionBegin
            ("channel.guest_star_session.end", "beta") -> return ChannelGuestStarSessionEnd
            ("channel.guest_star_guest.update", "beta") -> return ChannelGuestStarGuestUpdate
            ("channel.guest_star_settings.update", "beta") -> return ChannelGuestStarSettingsUpdate
            ("channel.guest_star_session.begin", "1") -> return ChannelGuestStarSessionBegin -- NOTE: the example json provided by twitch lists "1" as the version number
            ("channel.guest_star_session.end", "1") -> return ChannelGuestStarSessionEnd
            ("channel.guest_star_guest.update", "1") -> return ChannelGuestStarGuestUpdate
            ("channel.guest_star_settings.update", "1") -> return ChannelGuestStarSettingsUpdate
            ("channel.channel_points_automatic_reward_redemption.add", "1") -> return ChannelPointsAutomaticRewardRedemptionAddV1
            ("channel.channel_points_automatic_reward_redemption.add", "2") -> return ChannelPointsAutomaticRewardRedemptionAdd
            ("channel.channel_points_custom_reward.add", "1") -> return ChannelPointsCustomRewardAdd
            ("channel.channel_points_custom_reward.update", "1") -> return ChannelPointsCustomRewardUpdate
            ("channel.channel_points_custom_reward.remove", "1") -> return ChannelPointsCustomRewardRemove
            ("channel.channel_points_custom_reward_redemption.add", "1") -> return ChannelPointsCustomRewardRedemptionAdd
            ("channel.channel_points_custom_reward_redemption.update", "1") -> return ChannelPointsCustomRewardRedemptionUpdate
            ("channel.poll.begin", "1") -> return ChannelPollBegin
            ("channel.poll.progress", "1") -> return ChannelPollProgress
            ("channel.poll.end", "1") -> return ChannelPollEnd
            ("channel.prediction.begin", "1") -> return ChannelPredictionBegin
            ("channel.prediction.progress", "1") -> return ChannelPredictionProgress
            ("channel.prediction.lock", "1") -> return ChannelPredictionLock
            ("channel.prediction.end", "1") -> return ChannelPredictionEnd
            ("channel.suspicious_user.message", "1") -> return ChannelSuspiciousUserMessage
            ("channel.suspicious_user.update", "1") -> return ChannelSuspiciousUserUpdate
            ("channel.vip.add", "1") -> return ChannelVIPAdd
            ("channel.vip.remove", "1") -> return ChannelVIPRemove
            ("channel.warning.acknowledge", "1") -> return ChannelWarningAcknowledgement
            ("channel.warning.send", "1") -> return ChannelWarningSend
            ("channel.charity_campaign.donate", "1") -> return CharityDonation
            ("channel.charity_campaign.start", "1") -> return CharityCampaignStart
            ("channel.charity_campaign.progress", "1") -> return CharityCampaignProgress
            ("channel.charity_campaign.stop", "1") -> return CharityCampaignStop
            ("conduit.shard.disabled", "1") -> return ConduitShardDisabled
            ("drop.entitlement.grant", "1") -> return DropEntitlementGrant
            ("extension.bits_transaction.create", "1") -> return ExtensionBitsTransactionCreate
            ("channel.goal.begin", "1") -> return GoalBegin
            ("channel.goal.progress", "1") -> return GoalProgress
            ("channel.goal.end", "1") -> return GoalEnd
            ("channel.hype_train.begin", "1") -> return HypeTrainBegin
            ("channel.hype_train.progress", "1") -> return HypeTrainProgress
            ("channel.hype_train.end", "1") -> return HypeTrainEnd
            ("channel.shield_mode.begin", "1") -> return ShieldModeBegin
            ("channel.shield_mode.end", "1") -> return ShieldModeEnd
            ("channel.shoutout.create", "1") -> return ShoutoutCreate
            ("channel.shoutout.receive", "1") -> return ShoutoutReceived
            ("stream.online", "1") -> return StreamOnline
            ("stream.offline", "1") -> return StreamOffline
            ("user.authorization.grant", "1") -> return UserAuthorizationGrant
            ("user.authorization.revoke", "1") -> return UserAuthorizationRevoke
            ("user.update", "1") -> return UserUpdate
            ("user.whisper.message", "1") -> return WhisperReceived
            _ -> mzero

instance ToJSON Subscription where
    toJSON AutomodMessageHoldV1 = object [ "type"    .= String "automod.message.hold"
                                         , "version" .= String "1"
                                         ]
    toJSON AutomodMessageHold = object [ "type"    .= String "automod.message.hold"
                                       , "version" .= String "2"
                                       ]
    toJSON AutomodMessageUpdateV1 = object [ "type"    .= String "automod.message.update"
                                           , "version" .= String "1"
                                           ]
    toJSON AutomodMessageUpdate = object [ "type"    .= String "automod.message.update"
                                         , "version" .= String "2"
                                         ]
    toJSON AutomodSettingsUpdate = object [ "type"    .= String "automod.settings.update"
                                          , "version" .= String "1"
                                          ]
    toJSON AutomodTermsUpdate = object [ "type"    .= String "automod.terms.update"
                                       , "version" .= String "1"
                                       ]
    toJSON ChannelBitsUse = object [ "type"    .= String "channel.bits.use"
                                   , "version" .= String "1"
                                   ]
    toJSON ChannelUpdate = object [ "type"    .= String "channel.update"
                                  , "version" .= String "2"
                                  ]
    toJSON ChannelFollow = object [ "type"    .= String "channel.follow"
                                  , "version" .= String "2"
                                  ]
    toJSON ChannelAdBreakBegin = object [ "type"    .= String "channel.ad_break.begin"
                                        , "version" .= String "1"
                                        ]
    toJSON ChannelChatClear = object [ "type"    .= String "channel.chat.clear"
                                     , "version" .= String "1"
                                     ]
    toJSON ChannelChatClearUserMessages = object [ "type"    .= String "channel.chat.clear_user_messages"
                                                 , "version" .= String "1"
                                                 ]
    toJSON ChannelChatMessage = object [ "type"    .= String "channel.chat.message"
                                       , "version" .= String "1"
                                       ]
    toJSON ChannelChatMessageDelete = object [ "type"    .= String "channel.chat.message_delete"
                                             , "version" .= String "1"
                                             ]
    toJSON ChannelChatNotification = object [ "type"    .= String "channel.chat.notification"
                                            , "version" .= String "1"
                                            ]
    toJSON ChannelChatSettingsUpdate = object [ "type"    .= String "channel.chat_settings.update"
                                              , "version" .= String "1"
                                              ]
    toJSON ChannelChatUserMessageHold = object [ "type"    .= String "channel.chat.user_message_hold"
                                               , "version" .= String "1"
                                               ]
    toJSON ChannelChatUserMessageUpdate = object [ "type"    .= String "channel.chat.user_message_update"
                                                 , "version" .= String "1"
                                                 ]
    toJSON ChannelSharedChatSessionBegin = object [ "type"    .= String "channel.shared_chat.begin"
                                                  , "version" .= String "1"
                                                  ]
    toJSON ChannelSharedChatSessionUpdate = object [ "type"    .= String "channel.shared_chat.update"
                                                   , "version" .= String "1"
                                                   ]
    toJSON ChannelSharedChatSessionEnd = object [ "type"    .= String "channel.shared_chat.end"
                                                , "version" .= String "1"
                                                ]
    toJSON ChannelSubscribe = object [ "type"    .= String "channel.subscribe"
                                     , "version" .= String "1"
                                     ]
    toJSON ChannelSubscriptionEnd = object [ "type"    .= String "channel.subscription.end"
                                           , "version" .= String "1"
                                           ]
    toJSON ChannelSubscriptionGift = object [ "type"    .= String "channel.subscription.gift"
                                            , "version" .= String "1"
                                            ]
    toJSON ChannelSubscriptionMessage = object [ "type"    .= String "channel.subscription.message"
                                               , "version" .= String "1"
                                               ]
    toJSON ChannelCheer = object [ "type"    .= String "channel.cheer"
                                 , "version" .= String "1"
                                 ]
    toJSON ChannelRaid = object [ "type"    .= String "channel.raid"
                                , "version" .= String "1"
                                ]
    toJSON ChannelBan = object [ "type"    .= String "channel.ban"
                               , "version" .= String "1"
                               ]
    toJSON ChannelUnban = object [ "type"    .= String "channel.unban"
                                 , "version" .= String "1"
                                 ]
    toJSON ChannelUnbanRequestCreate = object [ "type"    .= String "channel.unban_request.create"
                                              , "version" .= String "1"
                                              ]
    toJSON ChannelUnbanRequestResolve = object [ "type"    .= String "channel.unban_request.resolve"
                                               , "version" .= String "1"
                                               ]
    toJSON ChannelModerateV1 = object [ "type"    .= String "channel.moderate"
                                      , "version" .= String "1"
                                      ]
    toJSON ChannelModerate = object [ "type"    .= String "channel.moderate"
                                    , "version" .= String "2"
                                    ]
    toJSON ChannelModeratorAdd = object [ "type"    .= String "channel.moderator.add"
                                        , "version" .= String "1"
                                        ]
    toJSON ChannelModeratorRemove = object [ "type"    .= String "channel.moderator.remove"
                                           , "version" .= String "1"
                                           ]
    toJSON ChannelGuestStarSessionBegin = object [ "type"    .= String "channel.guest_star_session.begin"
                                                 , "version" .= String "beta"
                                                 ]
    toJSON ChannelGuestStarSessionEnd = object [ "type"    .= String "channel.guest_star_session.end"
                                               , "version" .= String "beta"
                                               ]
    toJSON ChannelGuestStarGuestUpdate = object [ "type"    .= String "channel.guest_star_guest.update"
                                                , "version" .= String "beta"
                                                ]
    toJSON ChannelGuestStarSettingsUpdate = object [ "type"    .= String "channel.guest_star_settings.update"
                                                   , "version" .= String "beta"
                                                   ]
    toJSON ChannelPointsAutomaticRewardRedemptionAddV1 = object [ "type"    .= String "channel.channel_points_automatic_reward_redemption.add"
                                                                , "version" .= String "1"
                                                                ]
    toJSON ChannelPointsAutomaticRewardRedemptionAdd = object [ "type"    .= String "channel.channel_points_automatic_reward_redemption.add"
                                                              , "version" .= String "2"
                                                              ]
    toJSON ChannelPointsCustomRewardAdd = object [ "type"    .= String "channel.channel_points_custom_reward.add"
                                                 , "version" .= String "1"
                                                 ]
    toJSON ChannelPointsCustomRewardUpdate = object [ "type"    .= String "channel.channel_points_custom_reward.update"
                                                    , "version" .= String "1"
                                                    ]
    toJSON ChannelPointsCustomRewardRemove = object [ "type"    .= String "channel.channel_points_custom_reward.remove"
                                                    , "version" .= String "1"
                                                    ]
    toJSON ChannelPointsCustomRewardRedemptionAdd = object [ "type"    .= String "channel.channel_points_custom_reward_redemption.add"
                                                           , "version" .= String "1"
                                                           ]
    toJSON ChannelPointsCustomRewardRedemptionUpdate = object [ "type"    .= String "channel.channel_points_custom_reward_redemption.update"
                                                              , "version" .= String "1"
                                                              ]
    toJSON ChannelPollBegin = object [ "type"    .= String "channel.poll.begin"
                                     , "version" .= String "1"
                                     ]
    toJSON ChannelPollProgress = object [ "type"    .= String "channel.poll.progress"
                                        , "version" .= String "1"
                                        ]
    toJSON ChannelPollEnd = object [ "type"    .= String "channel.poll.end"
                                   , "version" .= String "1"
                                   ]
    toJSON ChannelPredictionBegin = object [ "type"    .= String "channel.prediction.begin"
                                           , "version" .= String "1"
                                           ]
    toJSON ChannelPredictionProgress = object [ "type"    .= String "channel.prediction.progress"
                                              , "version" .= String "1"
                                              ]
    toJSON ChannelPredictionLock = object [ "type"    .= String "channel.prediction.lock"
                                          , "version" .= String "1"
                                          ]
    toJSON ChannelPredictionEnd = object [ "type"    .= String "channel.prediction.end"
                                         , "version" .= String "1"
                                         ]
    toJSON ChannelSuspiciousUserMessage = object [ "type"    .= String "channel.suspicious_user.message"
                                                 , "version" .= String "1"
                                                 ]
    toJSON ChannelSuspiciousUserUpdate = object [ "type"    .= String "channel.suspicious_user.update"
                                                , "version" .= String "1"
                                                ]
    toJSON ChannelVIPAdd = object [ "type"    .= String "channel.vip.add"
                                  , "version" .= String "1"
                                  ]
    toJSON ChannelVIPRemove = object [ "type"    .= String "channel.vip.remove"
                                     , "version" .= String "1"
                                     ]
    toJSON ChannelWarningAcknowledgement = object [ "type"    .= String "channel.warning.acknowledge"
                                                  , "version" .= String "1"
                                                  ]
    toJSON ChannelWarningSend = object [ "type"    .= String "channel.warning.send"
                                       , "version" .= String "1"
                                       ]
    toJSON CharityDonation = object [ "type"    .= String "channel.charity_campaign.donate"
                                    , "version" .= String "1"
                                    ]
    toJSON CharityCampaignStart = object [ "type"    .= String "channel.charity_campaign.start"
                                         , "version" .= String "1"
                                         ]
    toJSON CharityCampaignProgress = object [ "type"    .= String "channel.charity_campaign.progress"
                                            , "version" .= String "1"
                                            ]
    toJSON CharityCampaignStop = object [ "type"    .= String "channel.charity_campaign.stop"
                                        , "version" .= String "1"
                                        ]
    toJSON ConduitShardDisabled = object [ "type"    .= String "conduit.shard.disabled"
                                         , "version" .= String "1"
                                         ]
    toJSON DropEntitlementGrant = object [ "type"    .= String "drop.entitlement.grant"
                                         , "version" .= String "1"
                                         ]
    toJSON ExtensionBitsTransactionCreate = object [ "type"    .= String "extension.bits_transaction.create"
                                                   , "version" .= String "1"
                                                   ]
    toJSON GoalBegin = object [ "type"    .= String "channel.goal.begin"
                              , "version" .= String "1"
                              ]
    toJSON GoalProgress = object [ "type"    .= String "channel.goal.progress"
                                 , "version" .= String "1"
                                 ]
    toJSON GoalEnd = object [ "type"    .= String "channel.goal.end"
                            , "version" .= String "1"
                            ]
    toJSON HypeTrainBegin = object [ "type"    .= String "channel.hype_train.begin"
                                   , "version" .= String "1"
                                   ]
    toJSON HypeTrainProgress = object [ "type"    .= String "channel.hype_train.progress"
                                      , "version" .= String "1"
                                      ]
    toJSON HypeTrainEnd = object [ "type"    .= String "channel.hype_train.end"
                                 , "version" .= String "1"
                                 ]
    toJSON ShieldModeBegin = object [ "type"    .= String "channel.shield_mode.begin"
                                    , "version" .= String "1"
                                    ]
    toJSON ShieldModeEnd = object [ "type"    .= String "channel.shield_mode.end"
                                  , "version" .= String "1"
                                  ]
    toJSON ShoutoutCreate = object [ "type"    .= String "channel.shoutout.create"
                                   , "version" .= String "1"
                                   ]
    toJSON ShoutoutReceived = object [ "type"    .= String "channel.shoutout.receive"
                                     , "version" .= String "1"
                                     ]
    toJSON StreamOnline = object [ "type"    .= String "stream.online"
                                 , "version" .= String "1"
                                 ]
    toJSON StreamOffline = object [ "type"    .= String "stream.offline"
                                  , "version" .= String "1"
                                  ]
    toJSON UserAuthorizationGrant = object [ "type"    .= String "user.authorization.grant"
                                           , "version" .= String "1"
                                           ]
    toJSON UserAuthorizationRevoke = object [ "type"    .= String "user.authorization.revoke"
                                            , "version" .= String "1"
                                            ]
    toJSON UserUpdate = object [ "type"    .= String "user.update"
                               , "version" .= String "1"
                               ]
    toJSON WhisperReceived = object [ "type"    .= String "user.whisper.message"
                                    , "version" .= String "1"
                                    ]
