{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Goal
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to moderation (ban/unban, moderator management, warnings)
-}

module Web.TwitchAPI.EventSub.Notification.Goal where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:)
                     , Object, withText
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Goal = Follow
          | Subscription
          | SubscriptionCount
          | NewSubscription
          | NewSubscriptionCount
          | NewBit
          | NewCheerer
          deriving ( Show, Eq )
instance FromJSON Goal where
    parseJSON = withText "Goal" $ \case
        "follow" -> return Follow
        "subscription" -> return Follow
        "subscription_count" -> return Follow
        "new_subscription" -> return Follow
        "new_subscription_count" -> return Follow
        "new_bit" -> return Follow
        "new_cheerer" -> return Follow
        _ -> mzero

data Message = Begin { eventID :: Text
                     , broadcaster :: User
                     , goal :: Goal
                     , current :: Integer
                     , target :: Integer
                     , description :: Text
                     , started :: Time.UTCTime
                     }
             | Progress { eventID :: Text
                        , broadcaster :: User
                        , goal :: Goal
                        , current :: Integer
                        , target :: Integer
                        , description :: Text
                        , started :: Time.UTCTime
                        }
             | End { eventID :: Text
                   , broadcaster :: User
                   , goal :: Goal
                   , current :: Integer
                   , target :: Integer
                   , description :: Text
                   , achieved :: Bool
                   , started :: Time.UTCTime
                   , ended :: Time.UTCTime
                   }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

begin :: MessageParser
begin o = do
    eventID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    goal <- o .: "type"
    current <- o .: "current_amount"
    target <- o .: "target_amount"
    description <- o .: "description"
    started <- o .: "started_at"
    return Begin{..}

progress :: MessageParser
progress o = do
    eventID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    goal <- o .: "type"
    current <- o .: "current_amount"
    target <- o .: "target_amount"
    description <- o .: "description"
    started <- o .: "started_at"
    return Progress{..}

end :: MessageParser
end o = do
    eventID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    goal <- o .: "type"
    current <- o .: "current_amount"
    target <- o .: "target_amount"
    description <- o .: "description"
    achieved <- o .: "achieved"
    started <- o .: "started_at"
    ended <- o .: "ended_at"
    return End{..}
