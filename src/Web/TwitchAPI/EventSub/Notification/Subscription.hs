{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Subscription
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to channel subscriptions
-}

module Web.TwitchAPI.EventSub.Notification.Subscription where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:), (.:?)
                     , Object, withObject, withText
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor, userFor' )

import qualified Web.TwitchAPI.EventSub.Notification.Emote as Emote

data SubMessage = SubMessage { messageBody :: Text
                             , emotes :: [Emote.Emote]
                             } deriving ( Show, Eq )
instance FromJSON SubMessage where
    parseJSON = withObject "Message" $ \o -> do
        messageBody <- o .: "text"
        emotes <- o .: "emotes"
        return SubMessage{..}

data SubscriptionTier = Prime
                      | Tier1
                      | Tier2
                      | Tier3
                      deriving ( Show, Eq )
instance FromJSON SubscriptionTier where
    parseJSON = withText "SubscriptionTier" $ \case
        "1000" -> return Tier1
        "2000" -> return Tier2
        "3000" -> return Tier3
        "Prime" -> return Prime
        _ -> mzero

data Message = Subscribe { broadcaster :: User
                         , user :: User
                         , tier :: SubscriptionTier
                         , gift :: Bool
                         }
             | SubscriptionEnd { broadcaster :: User
                               , user :: User
                               , tier :: SubscriptionTier
                               , gift :: Bool
                               }
             | SubscriptionGift { broadcaster :: User
                                , gifter :: Maybe User
                                , count :: Integer
                                , tier :: SubscriptionTier
                                , cumulativeTotal :: Maybe Integer
                                }
             | SubscriptionMessage { broadcaster :: User
                                   , user :: User
                                   , tier :: SubscriptionTier
                                   , message :: SubMessage
                                   , cumulativeMonths :: Integer
                                   , streakMonths :: Integer
                                   , duration :: Integer
                                   }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

subscribe :: MessageParser
subscribe o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    tier <- o .: "tier"
    gift <- o .: "is_gift"
    return Subscribe{..}

subscriptionEnd :: MessageParser
subscriptionEnd o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    tier <- o .: "tier"
    gift <- o .: "is_gift"
    return SubscriptionEnd{..}

subscriptionGift :: MessageParser
subscriptionGift o = do
    broadcaster <- userFor "broadcaster" o
    gifter <- userFor' "" o
    count <- o .: "total"
    tier <- o .: "tier"
    cumulativeTotal <- o .:? "cumulative_total"
    return SubscriptionGift{..}

subscriptionMessage :: MessageParser
subscriptionMessage o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    tier <- o .: "tier"
    message <- o .: "message"
    cumulativeMonths <- o .: "cumulative_months"
    streakMonths <- o .: "streak_months"
    duration <- o .: "duration_months"
    return SubscriptionMessage{..}
