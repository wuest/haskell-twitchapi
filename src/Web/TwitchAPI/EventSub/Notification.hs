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
import Data.Text     ( Text )

import Data.Aeson ( FromJSON(..), (.:), withObject
                  , ToJSON(..), (.=), object
                  , Value( String )
                  )

import Web.TwitchAPI.EventSub.Notification.User ( User )

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
import qualified Web.TwitchAPI.EventSub.Notification.ShieldMode     as ShieldMode
import qualified Web.TwitchAPI.EventSub.Notification.Shoutout       as Shoutout
import qualified Web.TwitchAPI.EventSub.Notification.Raid           as Raid
import qualified Web.TwitchAPI.EventSub.Notification.Status         as Status
import qualified Web.TwitchAPI.EventSub.Notification.StreamTogether as StreamTogether
import qualified Web.TwitchAPI.EventSub.Notification.Subscription   as Subscription
import qualified Web.TwitchAPI.EventSub.Notification.System         as System
import qualified Web.TwitchAPI.EventSub.Notification.User           as User
import qualified Web.TwitchAPI.EventSub.Notification.VIP            as VIP
import qualified Web.TwitchAPI.EventSub.Notification.Whisper        as Whisper

data Event = Automod Automod.Message
           | Chat Chat.Message
           | Moderation Moderation.Message
           | StreamTogether StreamTogether.Message
           | ChannelPoints ChannelPoints.Message
           | Polls Polls.Message
           | VIP VIP.Message
           | Charity Charity.Message
           | Bits Bits.Message
           | Subscription Subscription.Message
           | Drop Drop.Message
           | Goal Goal.Message
           | HypeTrain HypeTrain.Message
           | Status Status.Message
           | ShieldMode ShieldMode.Message
           | Shoutout Shoutout.Message
           | AdBreak AdBreak.Message
           | Raid Raid.Message
           | Whisper Whisper.Message
           | System System.Message
           deriving ( Show, Eq )
