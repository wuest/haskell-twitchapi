{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.HypeTrain
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to hype trains
-}

module Web.TwitchAPI.EventSub.Notification.HypeTrain where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:)
                     , Object, withObject, withText
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data ContributionType = Bits
                      | Subscription
                      | Other
                      deriving ( Show, Eq )
instance FromJSON ContributionType where
    parseJSON = withText "ContributionType" $ \case
        "bits" -> return Bits
        "subscription" -> return Subscription
        "other" -> return Other
        _ -> mzero

data Contribution = Contribution { user :: User
                                 , contributionType :: ContributionType
                                 , totalContributed :: Integer
                                 } deriving ( Show, Eq )
instance FromJSON Contribution where
    parseJSON = withObject "Contribution" $ \o -> do
        user <- userFor "" o
        contributionType <- o .: "type"
        totalContributed <- o .: "total"
        return Contribution{..}

data Message = Begin { trainID :: Text
                     , broadcaster :: User
                     , total :: Integer
                     , progress :: Integer
                     , goal :: Integer
                     , level :: Integer
                     , topContributions :: [Contribution]
                     , lastContribution :: Contribution
                     , started :: Time.UTCTime
                     , expries :: Time.UTCTime
                     , goldenKappa :: Bool
                     }
             | Progress { trainID :: Text
                        , broadcaster :: User
                        , total :: Integer
                        , progress :: Integer
                        , goal :: Integer
                        , level :: Integer
                        , topContributions :: [Contribution]
                        , lastContribution :: Contribution
                        , started :: Time.UTCTime
                        , expries :: Time.UTCTime
                        , goldenKappa :: Bool
                        }
             | End { trainID :: Text
                   , broadcaster :: User
                   , total :: Integer
                   , level :: Integer
                   , topContributions :: [Contribution]
                   , started :: Time.UTCTime
                   , ended :: Time.UTCTime
                   , cooldown :: Time.UTCTime
                   , goldenKappa :: Bool
                   }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

begin :: MessageParser
begin o = do
    trainID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    total <- o .: "total"
    progress <- o .: "progress"
    goal <- o .: "goal"
    level <- o .: "level"
    topContributions <- o .: "top_contributions"
    lastContribution <- o .: "last_contribution"
    started <- o .: "started_at"
    expries <- o .: "expires_at"
    goldenKappa <- o .: "is_golden_kappa_train"
    return Begin{..}

trainProgress :: MessageParser
trainProgress o = do
    trainID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    total <- o .: "total"
    progress <- o .: "progress"
    goal <- o .: "goal"
    level <- o .: "level"
    topContributions <- o .: "top_contributions"
    lastContribution <- o .: "last_contribution"
    started <- o .: "started_at"
    expries <- o .: "expires_at"
    goldenKappa <- o .: "is_golden_kappa_train"
    return Progress{..}

end :: MessageParser
end o = do
    trainID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    total <- o .: "total"
    level <- o .: "level"
    topContributions <- o .: "top_contributions"
    started <- o .: "started_at"
    ended <- o .: "ended_at"
    cooldown <- o .: "cooldown_ends_at"
    goldenKappa <- o .: "is_golden_kappa_train"
    return End{..}
