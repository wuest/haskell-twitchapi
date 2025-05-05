{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Whisper
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to whispers
-}

module Web.TwitchAPI.EventSub.Notification.Whisper where

import Prelude

import Data.Aeson    ( (.:), Object )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Message = Receive { from :: User
                       , to :: User
                       , whisperID :: Text
                       , body :: Text
                       }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

receive :: MessageParser
receive o = do
    from <- userFor "from" o
    to <- userFor "to" o
    whisperID <- o .: "whisper_id"
    whisper' <- o .: "whisper"
    body <- whisper' .: "text"
    return Receive{..}
