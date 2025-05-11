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

data Notification = AdBreak AdBreak.Message
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
instance FromJSON Notification where
    parseJSON = withObject "Notification " $ \o -> do
        sub <- o .: "subscription"
        eventType :: Text <- sub .: "type"
        eventVersion :: Text <- sub .: "version"
        event <- o .: "event"
        processNotification eventType eventVersion event

processNotification :: Text -> Text -> Object -> JSON.Types.Parser Notification 
processNotification "channel.ad_break.begin" "1" o = AdBreak.begin o <&> AdBreak

processNotification "automod.message.hold" "1" o = Automod.messageHoldV1 o <&> Automod
processNotification "automod.message.hold" "2" o = Automod.messageHold o <&> Automod
processNotification "automod.message.update" "1" o = Automod.messageUpdateV1 o <&> Automod
processNotification "automod.message.update" "2" o = Automod.messageUpdate o <&> Automod
processNotification "automod.settings.update" "1" o = Automod.settingsUpdate o <&> Automod
processNotification "automod.terms.update" "1" o = Automod.termsUpdate o <&> Automod

processNotification "channel.bits.use" "1" o = Bits.bitsUse o <&> Bits
processNotification "channel.cheer" "1" o = Bits.cheer o <&> Bits
processNotification "extension.bits_transaction.create" "1" o = Bits.extensionTransaction o <&> Bits

processNotification "channel.channel_points_automatic_reward_redemption.add" "1" o = ChannelPoints.automaticRewardRedemptionAddV1 o <&> ChannelPoints
processNotification "channel.channel_points_automatic_reward_redemption.add" "2" o = ChannelPoints.automaticRewardRedemptionAdd o <&> ChannelPoints
processNotification "channel.channel_points_custom_reward.add" "1" o = ChannelPoints.customRewardAdd o <&> ChannelPoints
processNotification "channel.channel_points_custom_reward.update" "1" o = ChannelPoints.customRewardUpdate o <&> ChannelPoints
processNotification "channel.channel_points_custom_reward.remove" "1" o = ChannelPoints.customRewardRemove o <&> ChannelPoints
processNotification "channel.channel_points_custom_reward_redemption.add" "1" o = ChannelPoints.customRewardRedemptionAdd o <&> ChannelPoints
processNotification "channel.channel_points_custom_reward_redemption.update" "1" o = ChannelPoints.customRewardRedemptionUpdate o <&> ChannelPoints

processNotification "channel.charity_campaign.donate" "1" o = Charity.donation o <&> Charity
processNotification "channel.charity_campaign.start" "1" o = Charity.campaignStart o <&> Charity
processNotification "channel.charity_campaign.progress" "1" o = Charity.campaignProgress o <&> Charity
processNotification "channel.charity_campaign.stop" "1" o = Charity.campaignStop o <&> Charity

processNotification "channel.chat.clear" "1" o = Chat.chatClear o <&> Chat
processNotification "channel.chat.clear_user_messages" "1" o = Chat.clearUserMessages o <&> Chat
processNotification "channel.chat.message" "1" o = Chat.chatMessage o <&> Chat
processNotification "channel.chat.message_delete" "1" o = Chat.chatMessageDelete o <&> Chat
processNotification "channel.chat.notification" "1" o = Chat.chatNotification o <&> Chat
processNotification "channel.chat_settings.update" "1" o = Chat.chatSettingsUpdate o <&> Chat
processNotification "channel.chat.user_message_hold" "1" o = Chat.userMessageHold o <&> Chat
processNotification "channel.chat.user_message_update" "1" o = Chat.userMessageUpdate o <&> Chat

processNotification "drop.entitlement.grant" "1" o = Drop.entitlementGrant o <&> Drop

processNotification "channel.follow" "2" o = Follow.follow o <&> Follow

processNotification "channel.goal.begin" "1" o = Goal.begin o <&> Goal
processNotification "channel.goal.progress" "1" o = Goal.progress o <&> Goal
processNotification "channel.goal.end" "1" o = Goal.end o <&> Goal

processNotification "channel.hype_train.begin" "1" o = HypeTrain.begin o <&> HypeTrain
processNotification "channel.hype_train.progress" "1" o = HypeTrain.trainProgress o <&> HypeTrain
processNotification "channel.hype_train.end" "1" o = HypeTrain.end o <&> HypeTrain

processNotification "channel.ban" "1" o = Moderation.banUser o <&> Moderation
processNotification "channel.unban" "1" o = Moderation.unbanUser o <&> Moderation
processNotification "channel.unban_request.create" "1" o = Moderation.unbanRequestCreate o <&> Moderation
processNotification "channel.unban_request.resolve" "1" o = Moderation.unbanRequestResolve o <&> Moderation
processNotification "channel.moderate" "1" o = Moderation.moderateV1 o <&> Moderation
processNotification "channel.moderate" "2" o = Moderation.moderate o <&> Moderation
processNotification "channel.moderator.add" "1" o = Moderation.moderatorAdd o <&> Moderation
processNotification "channel.moderator.remove" "1" o = Moderation.moderatorRemove o <&> Moderation
processNotification "channel.suspicious_user.message" "1" o = Moderation.suspiciousUserMessage o <&> Moderation
processNotification "channel.suspicious_user.update" "1" o = Moderation.suspiciousUserUpdate o <&> Moderation
processNotification "channel.warning.acknowledge" "1" o = Moderation.warningAckowledgement o <&> Moderation
processNotification "channel.warning.send" "1" o = Moderation.warningSend o <&> Moderation

processNotification "channel.poll.begin" "1" o = Polls.pollBegin o <&> Polls
processNotification "channel.poll.progress" "1" o = Polls.pollProgress o <&> Polls
processNotification "channel.poll.end" "1" o = Polls.pollEnd o <&> Polls
processNotification "channel.prediction.begin" "1" o = Polls.predictionBegin o <&> Polls
processNotification "channel.prediction.progress" "1" o = Polls.predictionProgress o <&> Polls
processNotification "channel.prediction.lock" "1" o = Polls.predictionLock o <&> Polls
processNotification "channel.prediction.end" "1" o = Polls.predictionEnd o <&> Polls

processNotification "channel.raid" "1" o = Raid.raid o <&> Raid

processNotification "channel.shield_mode.begin" "1" o = ShieldMode.begin o <&> ShieldMode
processNotification "channel.shield_mode.end" "1" o = ShieldMode.end o <&> ShieldMode

processNotification "channel.shoutout.create" "1" o = Shoutout.created o <&> Shoutout
processNotification "channel.shoutout.receive" "1" o = Shoutout.received o <&> Shoutout

processNotification "channel.update" "2" o = Status.channelUpdate o <&> Status
processNotification "stream.online" "1" o = Status.streamOnline o <&> Status
processNotification "stream.offline" "1" o = Status.streamOffline o <&> Status

processNotification "channel.shared_chat.begin" "1" o = StreamTogether.sharedChatSessionBegin o <&> StreamTogether
processNotification "channel.shared_chat.update" "1" o = StreamTogether.sharedChatSessionUpdate o <&> StreamTogether
processNotification "channel.shared_chat.end" "1" o = StreamTogether.sharedChatSessionEnd o <&> StreamTogether
processNotification "channel.guest_star_session.begin" "beta" o = StreamTogether.guestStarSessionBegin o <&> StreamTogether
processNotification "channel.guest_star_session.end" "beta" o = StreamTogether.guestStarSessionEnd o <&> StreamTogether
processNotification "channel.guest_star_guest.update" "beta" o = StreamTogether.guestStarGuestUpdate o <&> StreamTogether
processNotification "channel.guest_star_settings.update" "beta" o = StreamTogether.guestStarSettingsUpdate o <&> StreamTogether

processNotification "channel.subscribe" "1" o = Subscription.subscribe o <&> Subscription
processNotification "channel.subscription.end" "1" o = Subscription.subscriptionEnd o <&> Subscription
processNotification "channel.subscription.gift" "1" o = Subscription.subscriptionGift o <&> Subscription
processNotification "channel.subscription.message" "1" o = Subscription.subscriptionMessage o <&> Subscription

processNotification "channel.vip.add" "1" o = VIP.add o <&> VIP
processNotification "channel.vip.remove" "1" o = VIP.remove o <&> VIP

processNotification "user.whisper.message" "1" o = Whisper.receive o <&> Whisper

processNotification "conduit.shard.disabled" "1" o = System.conduitShardDisabled o <&> System
processNotification "user.authorization.grant" "1" o = System.userAuthorizationGrant o <&> System
processNotification "user.authorization.revoke" "1" o = System.userAuthorizationRevoke o <&> System
processNotification "user.update" "1" o = System.userUpdate o <&> System

processNotification _ _ _ = mzero
