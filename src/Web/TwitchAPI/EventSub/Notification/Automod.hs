{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Automod
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Automod functions, for re-export.
The Reason and Term types must be instantiated by the parser which has access to
its parent node, since that's where the text which is used to populate `term`
resides.
-}

module Web.TwitchAPI.EventSub.Notification.Automod where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:), (.:?)
                     , Object, withObject
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Text        as Text
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

import qualified Web.TwitchAPI.EventSub.Notification.Message as Message 

data Term = Term { termID :: Text
                 , boundary :: (Int, Int)
                 , term :: Text
                 , owner :: User
                 } deriving ( Show, Eq )
data Reason = Automod { category :: Category
                      , level :: Integer
                      , boundaries :: [(Int, Int)]
                      , terms :: [Text]
                      }
            | BlockedTerms [Term]
            deriving ( Show, Eq )
instance FromJSON Reason where
    parseJSON = withObject "event" $ \o -> do
        m <- o .: "message"
        body <- m .: "text"
        r :: Text <- o .: "reason"
        case r of
            "automod" -> do
                a <- o .: "automod"
                bounds <- a .: "boundaries"

                category <- a .: "category"
                level <- a .: "level"
                boundaries <- mapM parseBoundary bounds
                let terms = fmap (termFor body) boundaries
                return Automod{..}
            "blocked_term" -> do
                b <- o .: "blocked_term"
                ts <- b .: "terms_found"

                terms <- mapM (parseTerm body) ts
                return $ BlockedTerms terms
            _ -> mzero
      where
        parseBoundary :: Object -> JSON.Types.Parser (Int, Int)
        parseBoundary o = do
            start <- o .: "start_pos"
            end <- o .: "end_pos"
            return (start, end)
        parseTerm :: Text -> Object -> JSON.Types.Parser Term
        parseTerm body o = do
            termID <- o .: "term_id"
            bound <- o .: "boundary"
            boundary <- parseBoundary bound
            owner <- userFor "owner_broadcaster" o

            let term = termFor body boundary
            return Term{..}
        termFor :: Text -> (Int, Int) -> Text
        termFor body (start, end) = Text.take (end - start) $ Text.drop start body

data Category = Discrimination
              | Sexual
              | Hostility
              | Profanity
              | SmartDetection
              deriving ( Show, Eq )
instance FromJSON Category where
    parseJSON = withObject "automod" $ \o -> do
        c :: Text <- o .: "category"
        case c of
          "discrimination" -> return Discrimination
          "sexual" -> return Sexual
          "aggressive" -> return Hostility
          "profanity" -> return Profanity
          "smart" -> return SmartDetection
          _ -> mzero

data Level = Low
           | Medium
           | High
           | Max
           deriving ( Show, Eq, Enum, Bounded )
instance FromJSON Level where
    parseJSON = withObject "automod" $ \o -> do
        l :: Integer<- o .: "level"
        case l of
          1 -> return Low
          2 -> return Medium
          3 -> return High
          4 -> return Max
          _ -> mzero

data Status = Approved
            | Denied
            | Expired
            deriving ( Show, Eq )
instance FromJSON Status where
    parseJSON = withObject "event" $ \o -> do
        s :: Text <- o .: "status"
        case s of
          "approved" -> return Approved
          "denied" -> return Denied
          "expired" -> return Expired
          _ -> mzero

data Settings = Overall Integer
              | Settings { disability :: Integer
                         , aggression :: Integer
                         , sexuality :: Integer
                         , misogyny :: Integer
                         , bullying :: Integer
                         , swearing :: Integer
                         , race :: Integer
                         , sexBasedTerms :: Integer
                         } deriving ( Show, Eq )
instance FromJSON Settings where
    parseJSON = withObject "event" $ \o -> do
        overall :: Maybe Integer<- o .:? "overall_level"
        case overall of
          (Just l) -> return (Overall l)
          _ -> do
              disability <- o .: "disability"
              aggression <- o .: "aggression"
              sexuality <- o .: "sexuality_sex_or_gender"
              misogyny <- o .: "misogyny"
              bullying <- o .: "bullying"
              swearing <- o .: "swearing"
              race <- o .: "race_ethnicity_or_religion"
              sexBasedTerms <- o .: "sex_based_terms"
              return Settings{..}

data Action = AddPermitted
            | RemovePermitted
            | AddBlocked
            | RemoveBlocked
            deriving ( Show, Eq )
instance FromJSON Action where
    parseJSON = withObject "event" $ \o -> do
        a :: Text <- o .: "action"
        case a of
          "add_permitted" -> return AddPermitted
          "remove_permitted" -> return RemovePermitted
          "add_blocked" -> return AddBlocked
          "remove_blocked" -> return RemoveBlocked
          _ -> mzero

data Message = MessageHoldV1 { broadcaster :: User
                             , user :: User
                             , message :: Message.Message
                             , automodCategory :: Category
                             , automodLevel :: Level
                             , time :: Time.UTCTime
                             }
             | MessageHold { broadcaster :: User
                           , user :: User
                           , message :: Message.Message
                           , reason :: Reason
                           , time :: Time.UTCTime
                           }
             | MessageUpdateV1 { broadcaster :: User
                               , user :: User
                               , moderator :: User
                               , message :: Message.Message
                               , automodCategory :: Category
                               , automodLevel :: Level
                               , status :: Status
                               , time :: Time.UTCTime
                               }
             | MessageUpdate { broadcaster :: User
                             , user :: User
                             , moderator :: User
                             , message :: Message.Message
                             , status :: Status
                             , reason :: Reason
                             , time :: Time.UTCTime
                             }
             | SettingsUpdate { broadcaster :: User
                              , moderator :: User
                              , automodSettings :: [Settings]
                              }
             | TermsUpdate { broadcaster :: User
                           , moderator :: User
                           , action :: Action
                           , fromAutomod :: Bool
                           , updatedTerms :: [Text]
                           }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

messageHoldV1 :: MessageParser
messageHoldV1 o = do
    user <- userFor "" o
    broadcaster <- userFor "broadcaster" o
    e <- o .: "event"
    message <- o .: "event"
    time <- e .: "held_at"
    automodCategory <- e .: "category"
    automodLevel <- e .: "level"
    return MessageHoldV1{..}

messageHold :: MessageParser
messageHold o = do
    user <- userFor "" o
    broadcaster <- userFor "broadcaster" o
    e <- o .: "event"
    message <- o .: "event"
    time <- e .: "held_at"
    reason <- o .: "event"
    return MessageHold{..}

messageUpdateV1 :: MessageParser
messageUpdateV1 o = do
    user <- userFor "" o
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor "moderator" o
    e <- o .: "event"
    message <- o .: "event"
    time <- e .: "held_at"
    automodCategory <- e .: "category"
    automodLevel <- e .: "level"
    status <- e .: "status"
    return MessageUpdateV1{..}

messageUpdate :: MessageParser
messageUpdate o = do
    user <- userFor "" o
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor "moderator" o
    e <- o .: "event"
    message <- o .: "event"
    time <- e .: "held_at"
    status <- e .: "status"
    reason <- o .: "event"
    return MessageUpdate{..}

termsUpdate :: MessageParser
termsUpdate o = do
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor "moderator" o
    e <- o .: "event"
    fromAutomod <- e .: "from_automod"
    action <- e .: "action"
    updatedTerms <- e .: "terms"
    return TermsUpdate{..}
