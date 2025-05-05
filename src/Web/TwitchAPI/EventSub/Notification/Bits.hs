{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Bits
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to bits use (cheering, message effects)
-}

module Web.TwitchAPI.EventSub.Notification.Bits where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:)
                     , Object, withObject
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor, userFor' )

import qualified Web.TwitchAPI.EventSub.Notification.Message as Message

data Product = Product { productName :: Text
                       , sku :: Text
                       , spent :: Integer
                       , development :: Bool
                       } deriving ( Show, Eq )
instance FromJSON Product where
    parseJSON = withObject "Product" $ \o -> do
        productName <- o .: "name"
        sku <- o .: "sku"
        spent <- o .: "bits"
        development <- o .: "in_development"
        return Product{..}

data Emote = Emote { emoteID :: Text
                   , name :: Text
                   }
           deriving ( Show, Eq )
instance FromJSON Emote where
    parseJSON = withObject "Emote" $ \o -> do
        emoteID <- o .: "id"
        name <- o .: "name"
        return Emote{..}

data Use = UserCheer
         | MessageEffect Int
         | Celebration Emote
         | Gigantify Emote
         deriving ( Show, Eq )

data Message = BitsUse { broadcaster :: User
                       , user :: User
                       , bits :: Int
                       , use :: Use
                       , message :: Message.Message
                       }
              | Cheer { broadcaster :: User
                      , cheerer :: Maybe User
                      , body :: Text
                      , bits :: Int
                      }
              | ExtensionTransaction { extensionID :: Text
                                     , transactionID :: Text
                                     , broadcaster :: User
                                     , user :: User
                                     , extensionProduct :: Product
                                     }
              deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

bitsUse :: MessageParser
bitsUse o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    bits <- o .: "bits"
    useType :: Text <- o .: "type"
    use <- case useType of
               "cheer" -> return UserCheer
               "power_up" -> do
                   powerUp <- o .: "power_up"
                   powerUpType :: Text <- powerUp .: "type"
                   case powerUpType of
                       "message_effect" -> do
                           effectID <- powerUp .: "message_effect_id"
                           return $ MessageEffect effectID
                       "celebration" -> do
                           emote <- powerUp .: "emote"
                           emoteID <- emote .: "id"
                           name <- emote .: "name"
                           return $ Celebration $ Emote{..}
                       "gigantify_an_emote" -> do
                           emote <- powerUp .: "emote"
                           emoteID <- emote .: "id"
                           name <- emote .: "name"
                           return $ Gigantify $ Emote{..}
                       _ -> mzero
               _ -> mzero
    message <- o .: "message"
    return BitsUse{..}

cheer :: MessageParser
cheer o = do
    broadcaster <- userFor "broadcaster" o
    cheerer <- userFor' "" o
    body <- o .: "message"
    bits <- o .: "bits"
    return Cheer{..}

extensionTransaction :: MessageParser
extensionTransaction o = do
    extensionID <- o .: "extension_client_id"
    transactionID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    extensionProduct <- o .: "product"
    return ExtensionTransaction{..}
