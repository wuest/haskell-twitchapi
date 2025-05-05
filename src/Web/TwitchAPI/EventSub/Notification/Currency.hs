{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Currency
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Common currency functions
-}

module Web.TwitchAPI.EventSub.Notification.Currency where

import Prelude

import Data.Text  ( Text )
import Data.Aeson ( FromJSON(..), (.:), withObject )

data Currency = Currency { value :: Integer
                         , decimalPlaces :: Integer
                         , currency :: Text
                         } deriving ( Show, Eq )
instance FromJSON Currency where
    parseJSON = withObject "Currency" $ \o -> do
        value <- o .: "value"
        decimalPlaces <- o .: "decimal_places"
        currency <- o .: "currency"
        return Currency{..}
