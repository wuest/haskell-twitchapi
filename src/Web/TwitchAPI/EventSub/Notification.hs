{- |
Module      :  Web.TwitchAPI.EventSub.Notification
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable
-}

module Web.TwitchAPI.EventSub.Notification where

import Prelude

import Control.Monad ( mzero )
import Data.Functor  ( (<&>) )
import Data.Text     ( Text )

import Data.Aeson ( FromJSON(..), (.:), withObject
                  , Object
                  )

import qualified Data.Aeson.Types as JSON.Types

import qualified Web.TwitchAPI.EventSub.Notification.AdBreak        as AdBreak
import qualified Web.TwitchAPI.EventSub.Notification.Automod        as Automod
import qualified Web.TwitchAPI.EventSub.Notification.Bits           as Bits
import qualified Web.TwitchAPI.EventSub.Notification.ChannelPoints  as ChannelPoints
import qualified Web.TwitchAPI.EventSub.Notification.Charity        as Charity
import qualified Web.TwitchAPI.EventSub.Notification.Chat           as Chat
import qualified Web.TwitchAPI.EventSub.Notification.Drop           as Drop
import qualified Web.TwitchAPI.EventSub.Notification.Follow         as Follow
import qualified Web.TwitchAPI.EventSub.Notification.Goal           as Goal
import qualified Web.TwitchAPI.EventSub.Notification.HypeTrain      as HypeTrain
import qualified Web.TwitchAPI.EventSub.Notification.Moderation     as Moderation
import qualified Web.TwitchAPI.EventSub.Notification.Polls          as Polls
import qualified Web.TwitchAPI.EventSub.Notification.Raid           as Raid
import qualified Web.TwitchAPI.EventSub.Notification.ShieldMode     as ShieldMode
import qualified Web.TwitchAPI.EventSub.Notification.Shoutout       as Shoutout
import qualified Web.TwitchAPI.EventSub.Notification.Status         as Status
import qualified Web.TwitchAPI.EventSub.Notification.StreamTogether as StreamTogether
import qualified Web.TwitchAPI.EventSub.Notification.Subscription   as Subscription
import qualified Web.TwitchAPI.EventSub.Notification.System         as System
import qualified Web.TwitchAPI.EventSub.Notification.VIP            as VIP
import qualified Web.TwitchAPI.EventSub.Notification.Whisper        as Whisper

data Event = AdBreak AdBreak.Message
           | Automod Automod.Message
           | Bits Bits.Message
           | ChannelPoints ChannelPoints.Message
           | Charity Charity.Message
           | Chat Chat.Message
           | Drop Drop.Message
           | Follow Follow.Message
           | Goal Goal.Message
           | HypeTrain HypeTrain.Message
           | Moderation Moderation.Message
           | Polls Polls.Message
           | Raid Raid.Message
           | ShieldMode ShieldMode.Message
           | Shoutout Shoutout.Message
           | Status Status.Message
           | StreamTogether StreamTogether.Message
           | Subscription Subscription.Message
           | VIP VIP.Message
           | Whisper Whisper.Message
           | System System.Message
           deriving ( Show, Eq )
instance FromJSON Event where
    parseJSON = withObject "Event" $ \o -> do
        sub <- o .: "subscription"
        eventType :: Text <- sub .: "type"
        eventVersion :: Text <- sub .: "version"
        event <- o .: "event"
        processEvent eventType eventVersion event

processEvent :: Text -> Text -> Object -> JSON.Types.Parser Event
processEvent "channel.ad_break.begin" "1" o = AdBreak.begin o <&> AdBreak

processEvent "automod.message.hold" "1" o = Automod.messageHoldV1 o <&> Automod
processEvent "automod.message.hold" "2" o = Automod.messageHold o <&> Automod
processEvent "automod.message.update" "1" o = Automod.messageUpdateV1 o <&> Automod
processEvent "automod.message.update" "2" o = Automod.messageUpdate o <&> Automod
processEvent "automod.settings.update" "1" o = Automod.settingsUpdate o <&> Automod
processEvent "automod.terms.update" "1" o = Automod.termsUpdate o <&> Automod

processEvent "channel.bits.use" "1" o = Bits.bitsUse o <&> Bits
processEvent "channel.cheer" "1" o = Bits.cheer o <&> Bits
processEvent "extension.bits_transaction.create" "1" o = Bits.extensionTransaction o <&> Bits

processEvent "channel.channel_points_automatic_reward_redemption.add" "1" o = ChannelPoints.automaticRewardRedemptionAddV1 o <&> ChannelPoints
processEvent "channel.channel_points_automatic_reward_redemption.add" "2" o = ChannelPoints.automaticRewardRedemptionAdd o <&> ChannelPoints
processEvent "channel.channel_points_custom_reward.add" "1" o = ChannelPoints.customRewardAdd o <&> ChannelPoints
processEvent "channel.channel_points_custom_reward.update" "1" o = ChannelPoints.customRewardUpdate o <&> ChannelPoints
processEvent "channel.channel_points_custom_reward.remove" "1" o = ChannelPoints.customRewardRemove o <&> ChannelPoints
processEvent "channel.channel_points_custom_reward_redemption.add" "1" o = ChannelPoints.customRewardRedemptionAdd o <&> ChannelPoints
processEvent "channel.channel_points_custom_reward_redemption.update" "1" o = ChannelPoints.customRewardRedemptionUpdate o <&> ChannelPoints

processEvent "channel.charity_campaign.donate" "1" o = Charity.donation o <&> Charity
processEvent "channel.charity_campaign.start" "1" o = Charity.campaignStart o <&> Charity
processEvent "channel.charity_campaign.progress" "1" o = Charity.campaignProgress o <&> Charity
processEvent "channel.charity_campaign.stop" "1" o = Charity.campaignStop o <&> Charity

processEvent "channel.chat.clear" "1" o = Chat.chatClear o <&> Chat
processEvent "channel.chat.clear_user_messages" "1" o = Chat.clearUserMessages o <&> Chat
processEvent "channel.chat.message" "1" o = Chat.chatMessage o <&> Chat
processEvent "channel.chat.message_delete" "1" o = Chat.chatMessageDelete o <&> Chat
processEvent "channel.chat.notification" "1" o = Chat.chatNotification o <&> Chat
processEvent "channel.chat_settings.update" "1" o = Chat.chatSettingsUpdate o <&> Chat
processEvent "channel.chat.user_message_hold" "1" o = Chat.userMessageHold o <&> Chat
processEvent "channel.chat.user_message_update" "1" o = Chat.userMessageUpdate o <&> Chat

processEvent "drop.entitlement.grant" "1" o = Drop.entitlementGrant o <&> Drop

processEvent "channel.follow" "2" o = Follow.follow o <&> Follow

processEvent "channel.goal.begin" "1" o = Goal.begin o <&> Goal
processEvent "channel.goal.progress" "1" o = Goal.progress o <&> Goal
processEvent "channel.goal.end" "1" o = Goal.end o <&> Goal

processEvent "channel.hype_train.begin" "1" o = HypeTrain.begin o <&> HypeTrain
processEvent "channel.hype_train.progress" "1" o = HypeTrain.trainProgress o <&> HypeTrain
processEvent "channel.hype_train.end" "1" o = HypeTrain.end o <&> HypeTrain

processEvent "channel.ban" "1" o = Moderation.banUser o <&> Moderation
processEvent "channel.unban" "1" o = Moderation.unbanUser o <&> Moderation
processEvent "channel.unban_request.create" "1" o = Moderation.unbanRequestCreate o <&> Moderation
processEvent "channel.unban_request.resolve" "1" o = Moderation.unbanRequestResolve o <&> Moderation
processEvent "channel.moderate" "1" o = Moderation.moderateV1 o <&> Moderation
processEvent "channel.moderate" "2" o = Moderation.moderate o <&> Moderation
processEvent "channel.moderator.add" "1" o = Moderation.moderatorAdd o <&> Moderation
processEvent "channel.moderator.remove" "1" o = Moderation.moderatorRemove o <&> Moderation
processEvent "channel.suspicious_user.message" "1" o = Moderation.suspiciousUserMessage o <&> Moderation
processEvent "channel.suspicious_user.update" "1" o = Moderation.suspiciousUserUpdate o <&> Moderation
processEvent "channel.warning.acknowledge" "1" o = Moderation.warningAckowledgement o <&> Moderation
processEvent "channel.warning.send" "1" o = Moderation.warningSend o <&> Moderation

processEvent "channel.poll.begin" "1" o = Polls.pollBegin o <&> Polls
processEvent "channel.poll.progress" "1" o = Polls.pollProgress o <&> Polls
processEvent "channel.poll.end" "1" o = Polls.pollEnd o <&> Polls
processEvent "channel.prediction.begin" "1" o = Polls.predictionBegin o <&> Polls
processEvent "channel.prediction.progress" "1" o = Polls.predictionProgress o <&> Polls
processEvent "channel.prediction.lock" "1" o = Polls.predictionLock o <&> Polls
processEvent "channel.prediction.end" "1" o = Polls.predictionEnd o <&> Polls

processEvent "channel.raid" "1" o = Raid.raid o <&> Raid

processEvent "channel.shield_mode.begin" "1" o = ShieldMode.begin o <&> ShieldMode
processEvent "channel.shield_mode.end" "1" o = ShieldMode.end o <&> ShieldMode

processEvent "channel.shoutout.create" "1" o = Shoutout.created o <&> Shoutout
processEvent "channel.shoutout.receive" "1" o = Shoutout.received o <&> Shoutout

processEvent "channel.update" "2" o = Status.channelUpdate o <&> Status
processEvent "stream.online" "1" o = Status.streamOnline o <&> Status
processEvent "stream.offline" "1" o = Status.streamOffline o <&> Status

processEvent "channel.shared_chat.begin" "1" o = StreamTogether.sharedChatSessionBegin o <&> StreamTogether
processEvent "channel.shared_chat.update" "1" o = StreamTogether.sharedChatSessionUpdate o <&> StreamTogether
processEvent "channel.shared_chat.end" "1" o = StreamTogether.sharedChatSessionEnd o <&> StreamTogether
processEvent "channel.guest_star_session.begin" "beta" o = StreamTogether.guestStarSessionBegin o <&> StreamTogether
processEvent "channel.guest_star_session.end" "beta" o = StreamTogether.guestStarSessionEnd o <&> StreamTogether
processEvent "channel.guest_star_guest.update" "beta" o = StreamTogether.guestStarGuestUpdate o <&> StreamTogether
processEvent "channel.guest_star_settings.update" "beta" o = StreamTogether.guestStarSettingsUpdate o <&> StreamTogether

processEvent "channel.subscribe" "1" o = Subscription.subscribe o <&> Subscription
processEvent "channel.subscription.end" "1" o = Subscription.subscriptionEnd o <&> Subscription
processEvent "channel.subscription.gift" "1" o = Subscription.subscriptionGift o <&> Subscription
processEvent "channel.subscription.message" "1" o = Subscription.subscriptionMessage o <&> Subscription

processEvent "channel.vip.add" "1" o = VIP.add o <&> VIP
processEvent "channel.vip.remove" "1" o = VIP.remove o <&> VIP

processEvent "user.whisper.message" "1" o = Whisper.receive o <&> Whisper

processEvent "conduit.shard.disabled" "1" o = System.conduitShardDisabled o <&> System
processEvent "user.authorization.grant" "1" o = System.userAuthorizationGrant o <&> System
processEvent "user.authorization.revoke" "1" o = System.userAuthorizationRevoke o <&> System
processEvent "user.update" "1" o = System.userUpdate o <&> System

processEvent _ _ _ = mzero
