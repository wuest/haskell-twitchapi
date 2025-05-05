{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Emote
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Emotes with position
-}

module Web.TwitchAPI.EventSub.Notification.Emote where

import Prelude

import Data.Aeson    ( FromJSON(..), (.:), withObject )
import Data.Text     ( Text )

data Emote = Emote { emoteID :: Text
                   , begin :: Int
                   , end :: Int
                   } deriving ( Show, Eq )
instance FromJSON Emote where
    parseJSON = withObject "RewardChatEmote" $ \o -> do
        emoteID <- o .: "id"
        begin <- o .: "begin"
        end <- o .: "end"
        return Emote{..}
