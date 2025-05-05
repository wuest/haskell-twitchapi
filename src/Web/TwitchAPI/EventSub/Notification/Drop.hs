{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Drop
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to viewer drops
-}

module Web.TwitchAPI.EventSub.Notification.Drop where

import Prelude

import Data.Aeson ( (.:), Object )
import Data.Text  ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

data Message = EntitlementGrant { eventID :: Text
                                , organizationID :: Text
                                , categoryID :: Text
                                , categoryName :: Text
                                , campaignID :: Text
                                , user :: User
                                , entitlementID
                                , benefitID
                                , created :: Time.UTCTime
                                } deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

entitlementGrant :: MessageParser
entitlementGrant o = do
    eventID <- o .: "id"
    d <- o .: "data"
    organizationID <- d .: "organization_id"
    categoryID <- d .: "category_id"
    categoryName <- d .: "category_name"
    campaignID <- d .: "campaign_id"
    user <- userFor "" d
    entitlementID <- d .: "entitlement_id"
    benefitID <- d .: "benefit_id"
    created <- d .: "create_at"
    return EntitlementGrant{..}
