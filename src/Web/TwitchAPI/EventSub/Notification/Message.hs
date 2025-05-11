{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Message
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable
-}

module Web.TwitchAPI.EventSub.Notification.Message where

import Prelude

import Control.Monad ( mzero )
import Data.Text     ( Text )
import Data.Maybe    ( fromMaybe )

import Data.Aeson ( FromJSON(..), (.:), (.:?), Object, withObject )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.User()

data Badge = Badge { badgeSet :: Text
                   , badgeID :: Text
                   , info :: Text
                   } deriving ( Show, Eq )
instance FromJSON Badge where
    parseJSON = withObject "Badge" $ \o -> do
        badgeSet <- o .: "set_id"
        badgeID <- o .: "id"
        info <- o .: "info"
        return Badge{..}

data MessageType = Normal
                 | ChannelPointsHighlight
                 | ChannelPointsSubOnly
                 | UserIntro
                 | PowerUpMessageEffect
                 | PowerUpGigantifiedEmote
                 deriving ( Show, Eq )
instance FromJSON MessageType where
    parseJSON = withObject "MessageType" $ \o -> do
        mt :: Text <- o .: "message_type"
        case mt of
          "text" -> return Normal
          "channel_points_highlighted" -> return ChannelPointsHighlight
          "channel_points_sub_only" -> return ChannelPointsSubOnly
          "user_intro" -> return UserIntro
          "power_ups_message_effect" -> return PowerUpMessageEffect
          "power_ups_gigantified_emote" -> return PowerUpGigantifiedEmote
          _ -> mzero

data Fragment = Emote { text :: Text
                      , emoteID :: Text
                      , emoteSet :: Text
                      }
              | Cheermote { text :: Text
                          , prefix :: Text
                          , bits :: Integer
                          , tier :: Integer
                          }
              | PlainText Text
              deriving ( Show, Eq )
instance FromJSON Fragment where
    parseJSON = withObject "fragment" $ \o -> do
        text <- o .: "text"
        fragmentType :: Text <- o .: "type"
        case fragmentType of
          "emote" -> do
              e <- o .: "emote"
              emoteID <- e .: "id"
              emoteSet <- e .: "emote_set_id"
              return Emote{..}
          "cheermote" -> do
              e <- o .: "cheermote"
              prefix <- e .: "prefix"
              bits <- e .: "bits"
              tier <- e .: "tier"
              return Cheermote{..}
          "text" -> return $ PlainText text
          _ -> mzero

data Message = Message { body :: Text
                       , messageID :: Text
                       , fragments :: [Fragment]
                       }
             | PartialMessage { body :: Text
                              , fragments :: [Fragment]
                              }
             deriving ( Show, Eq )
instance FromJSON Message where
    parseJSON = withObject "Message" fromEvent

fromEvent :: Object -> JSON.Types.Parser Message
fromEvent e = do
    messageID' <- e .:? "message_id"
    message' <- e .:? "message"
    let base = fromMaybe e message'
    case messageID' of
        Just messageID -> withID messageID base
        Nothing -> withoutID base

withID :: Text -> Object -> JSON.Types.Parser Message
withID messageID o = do
    body <- o .: "text"
    fragments <- o .: "fragments"
    return Message{..}

withoutID :: Object -> JSON.Types.Parser Message
withoutID o = do
    body <- o .: "text"
    fragments <- o .: "fragments"
    return PartialMessage{..}
