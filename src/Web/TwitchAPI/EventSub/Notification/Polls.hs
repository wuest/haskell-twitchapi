{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Polls
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to polls and predictions
-}

module Web.TwitchAPI.EventSub.Notification.Polls where

import Prelude

import Control.Monad ( mzero )
import Data.Text     ( Text )

import Data.Aeson ( FromJSON(..), (.:), (.:?), Object, withObject, withText )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User         ( User, userFor )

data Choice = Choice { choiceID :: Text
                     , choiceTitle :: Text
                     , bitsVotes :: Integer
                     , channelPointsVotes :: Integer
                     , votes :: Integer
                     } deriving ( Show, Eq )
instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \o -> do
        choiceID <- o .: "id"
        choiceTitle <- o .: "title"
        bitsVotes <- o .: "bits_votes"
        channelPointsVotes <- o .: "channel_points_votes"
        votes <- o .: "votes"
        return Choice{..}

data Color = Pink
           | Blue
           deriving ( Show, Eq )
instance FromJSON Color where
    parseJSON = withText "Color" $ \case
        "pink" -> return Pink
        "blue" -> return Blue
        _ -> mzero

data Predictor = Predictor { user :: User
                           , won :: Maybe Integer
                           , used :: Integer
                           } deriving ( Show, Eq )
instance FromJSON Predictor where
    parseJSON = withObject "Predictor" $ \o -> do
        user <- userFor "" o
        won <- o .: "channel_points_won"
        used <- o .: "channel_points_used"
        return Predictor{..}

data Outcome = Outcome { outcomeID :: Text
                       , outcomeTitle :: Text
                       , color :: Color
                       , userCount :: Integer
                       , channelPoints :: Integer
                       , topPredictors :: [Predictor]
                       } deriving ( Show, Eq )
instance FromJSON Outcome where
    parseJSON = withObject "Outcome" $ \o -> do
        outcomeID <- o .: "id"
        outcomeTitle <- o .: "title"
        color <- o .: "color"
        userCount <- o .: "users"
        channelPoints <- o .: "channel_points"
        topPredictors <- o .: "top_predictors"
        return Outcome{..}

data PredictionStatus = Resolved
                      | Canceled
                      deriving ( Show, Eq )
instance FromJSON PredictionStatus where
    parseJSON = withText "Status" $ \case
        "resolved" -> return Resolved
        "canceled" -> return Canceled
        _ -> mzero

data PollStatus = Completed
                | Archived
                | Terminated
                deriving ( Show, Eq )
instance FromJSON PollStatus where
    parseJSON = withText "Status" $ \case
        "completed" -> return Completed
        "archived" -> return Archived
        "terminated" -> return Terminated
        _ -> mzero

data Message = PollBegin { pollID :: Text
                         , broadcaster :: User
                         , title :: Text
                         , choices :: [Choice]
                         , bitsVoting :: Maybe Integer
                         , channelPointsVoting :: Maybe Integer
                         , startTime :: Time.UTCTime
                         , endTime :: Time.UTCTime
                         }
             | PollProgress { pollID :: Text
                            , broadcaster :: User
                            , title :: Text
                            , choices :: [Choice]
                            , bitsVoting :: Maybe Integer
                            , channelPointsVoting :: Maybe Integer
                            , startTime :: Time.UTCTime
                            , endTime :: Time.UTCTime
                            }
             | PollEnd { pollID :: Text
                       , broadcaster :: User
                       , title :: Text
                       , choices :: [Choice]
                       , bitsVoting :: Maybe Integer
                       , channelPointsVoting :: Maybe Integer
                       , pollStatus :: PollStatus
                       , startTime :: Time.UTCTime
                       , endTime :: Time.UTCTime
                       }
             | PredictionBegin { predictionID :: Text
                               , broadcaster :: User
                               , title :: Text
                               , outcomes :: [Outcome]
                               , startTime :: Time.UTCTime
                               , lockTime :: Time.UTCTime
                               }
             | PredictionProgress { predictionID :: Text
                                  , broadcaster :: User
                                  , title :: Text
                                  , outcomes :: [Outcome]
                                  , startTime :: Time.UTCTime
                                  , lockTime :: Time.UTCTime
                                  }
             | PredictionLock { predictionID :: Text
                              , broadcaster :: User
                              , title :: Text
                              , outcomes :: [Outcome]
                              , startTime :: Time.UTCTime
                              , lockTime :: Time.UTCTime
                              }
             | PredictionEnd { predictionID :: Text
                             , broadcaster :: User
                             , title :: Text
                             , winningOutcome :: Text
                             , outcomes :: [Outcome]
                             , predictionStatus :: PredictionStatus
                             , startTime :: Time.UTCTime
                             , endTime :: Time.UTCTime
                             }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

pollBegin :: MessageParser
pollBegin o = do
    pollID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    choices <- o .: "choices"

    bitsVoting' <- o .: "bits_voting"
    bitsVotingEnabled <- bitsVoting' .: "is_enabled"
    bitsVoting <- if bitsVotingEnabled then bitsVoting' .:? "amount_per_vote"
                                       else return Nothing
    channelPointsVoting' <- o .: "channel_points_voting"
    channelPointsVotingEnabled <- channelPointsVoting' .: "is_enabled"
    channelPointsVoting <- if channelPointsVotingEnabled then channelPointsVoting' .:? "amount_per_vote"
                                                         else return Nothing

    startTime <- o .: "started_at"
    endTime <- o .: "ends_at"
    return PollBegin{..}

pollProgress :: MessageParser
pollProgress o = do
    pollID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    choices <- o .: "choices"

    bitsVoting' <- o .: "bits_voting"
    bitsVotingEnabled <- bitsVoting' .: "is_enabled"
    bitsVoting <- if bitsVotingEnabled then bitsVoting' .:? "amount_per_vote"
                                       else return Nothing
    channelPointsVoting' <- o .: "channel_points_voting"
    channelPointsVotingEnabled <- channelPointsVoting' .: "is_enabled"
    channelPointsVoting <- if channelPointsVotingEnabled then channelPointsVoting' .:? "amount_per_vote"
                                                         else return Nothing

    startTime <- o .: "started_at"
    endTime <- o .: "ends_at"
    return PollProgress{..}

pollEnd :: MessageParser
pollEnd o = do
    pollID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    choices <- o .: "choices"

    bitsVoting' <- o .: "bits_voting"
    bitsVotingEnabled <- bitsVoting' .: "is_enabled"
    bitsVoting <- if bitsVotingEnabled then bitsVoting' .:? "amount_per_vote"
                                       else return Nothing
    channelPointsVoting' <- o .: "channel_points_voting"
    channelPointsVotingEnabled <- channelPointsVoting' .: "is_enabled"
    channelPointsVoting <- if channelPointsVotingEnabled then channelPointsVoting' .:? "amount_per_vote"
                                                         else return Nothing

    pollStatus <- o .: "status"
    startTime <- o .: "started_at"
    endTime <- o .: "ended_at"
    return PollEnd{..}
    
predictionBegin :: MessageParser
predictionBegin o = do
    predictionID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    outcomes <- o .: "outcomes"
    startTime <- o .: "started_at"
    lockTime <- o .: "locks_at"
    return PredictionBegin{..}
    
predictionProgress :: MessageParser
predictionProgress o = do
    predictionID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    outcomes <- o .: "outcomes"
    startTime <- o .: "started_at"
    lockTime <- o .: "locks_at"
    return PredictionProgress{..}
    
predictionLock :: MessageParser
predictionLock o = do
    predictionID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    outcomes <- o .: "outcomes"
    startTime <- o .: "started_at"
    lockTime <- o .: "locked_at"
    return PredictionLock{..}
    
predictionEnd :: MessageParser
predictionEnd o = do
    predictionID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    title <- o .: "title"
    winningOutcome <- o .: "winning_outcome_id"
    outcomes <- o .: "outcomes"
    predictionStatus <- o .: "status"
    startTime <- o .: "started_at"
    endTime <- o .: "ended_at"
    return PredictionEnd{..}
