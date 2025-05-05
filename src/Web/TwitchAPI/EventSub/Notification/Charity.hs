{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Charity
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to charity info (donations, campaign status)
-}

module Web.TwitchAPI.EventSub.Notification.Charity where

import Prelude

import Data.Aeson    ( (.:), Object )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

import qualified Web.TwitchAPI.EventSub.Notification.Currency as Currency

data Charity = Charity { name :: Text
                       , description :: Text
                       , logo :: Text
                       , website :: Text
                       } deriving ( Show, Eq )

data Message = Donation { donationID :: Text
                        , campaignID :: Text
                        , broadcaster :: User
                        , user :: User
                        , charity :: Charity
                        , amount :: Currency.Currency
                        }
             | CampaignStart { donationID :: Text
                             , campaignID :: Text
                             , broadcaster :: User
                             , charity :: Charity
                             , amount :: Currency.Currency
                             }
             | CampaignProgress { donationID :: Text
                                , campaignID :: Text
                                , broadcaster :: User
                                , charity :: Charity
                                , amount :: Currency.Currency
                                }
             | CampaignStop { donationID :: Text
                            , campaignID :: Text
                            , broadcaster :: User
                            , charity :: Charity
                            , amount :: Currency.Currency
                            }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

donation :: MessageParser
donation o = do
    donationID <- o .: "id"
    campaignID <- o .: "campaign_id"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    name <- o .: "charity_name"
    description <- o .: "charity_description"
    logo <- o .: "charity_logo"
    website <- o .: "charity_website"
    amount <- o .: "amount"
    let charity = Charity{..}
    return Donation{..}

campaignStart :: MessageParser
campaignStart o = do
    donationID <- o .: "id"
    campaignID <- o .: "campaign_id"
    broadcaster <- userFor "broadcaster" o
    name <- o .: "charity_name"
    description <- o .: "charity_description"
    logo <- o .: "charity_logo"
    website <- o .: "charity_website"
    amount <- o .: "amount"
    let charity = Charity{..}
    return CampaignStart{..}

campaignProgress :: MessageParser
campaignProgress o = do
    donationID <- o .: "id"
    campaignID <- o .: "campaign_id"
    broadcaster <- userFor "broadcaster" o
    name <- o .: "charity_name"
    description <- o .: "charity_description"
    logo <- o .: "charity_logo"
    website <- o .: "charity_website"
    amount <- o .: "amount"
    let charity = Charity{..}
    return CampaignProgress{..}

campaignStop :: MessageParser
campaignStop o = do
    donationID <- o .: "id"
    campaignID <- o .: "campaign_id"
    broadcaster <- userFor "broadcaster" o
    name <- o .: "charity_name"
    description <- o .: "charity_description"
    logo <- o .: "charity_logo"
    website <- o .: "charity_website"
    amount <- o .: "amount"
    let charity = Charity{..}
    return CampaignStop{..}
