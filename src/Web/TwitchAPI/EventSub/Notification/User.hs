{- |
Module      :  Web.TwitchAPI.EventSub.Notification.User
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

User datum, for re-export.
Due to variance in node names which constitute a user object, this cannot have
be an instance of FromJSON directly.
-}

module Web.TwitchAPI.EventSub.Notification.User where

import Prelude

import Data.Aeson ( (.:), (.:?), Object )
import Data.Text  ( Text )

import qualified Data.Aeson.Key   as JSON
import qualified Data.Aeson.Types as JSON.Types

data User = User { userID :: Text
                 , login :: Text
                 , username :: Text
                 } deriving ( Show, Eq )

userFor :: String -> Object -> JSON.Types.Parser User
userFor t o = do
    let t' = case t of
                 "" -> ""
                 _ -> t ++ "_"
    uid <- o .: JSON.fromString (t' ++ "user_id")
    ul <- o .: JSON.fromString (t' ++ "user_login")
    un <- o .: JSON.fromString (t' ++ "user_name")
    return $ User uid ul un

userFor' :: String -> Object -> JSON.Types.Parser (Maybe User)
userFor' t o = do
    let t' = case t of
                 "" -> ""
                 _ -> t ++ "_"
    uid <- o .:? JSON.fromString (t' ++ "user_id")
    ul <- o .:? JSON.fromString (t' ++ "user_login")
    un <- o .:? JSON.fromString (t' ++ "user_name")
    return $ User <$> uid <*> ul <*> un
