{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.TwitchAPI.Helix.Bits where

import Prelude

import qualified Data.Time           as Time
import qualified Data.Time.RFC3339   as Time ( formatTimeRFC3339, parseTimeRFC3339 )
import qualified Data.Text           as Text

import Data.Aeson ( FromJSON(..), (.:), withObject
                  , ToJSON(..), (.=), object
                  , Object
                  )

leaderboardScope :: String
leaderboardScope = "bits:read"

leaderboardRequest :: (String, String)
leaderboardRequest = ("GET", "https://api.twitch.tv/helix/bits/leaderboard")

data Period = Day | Week | Month | Year | All deriving ( Eq )
instance Show Period where
    show Day   = "day"
    show Week  = "week"
    show Month = "month"
    show Year  = "year"
    show All   = "all"

data Leaderboard = Leaderboard { count     :: Maybe Integer
                               , period    :: Maybe Period
                               , startedAt :: Maybe Time.UTCTime
                               , userId    :: Maybe Integer
                               } deriving ( Show, Eq )

instance ToJSON Leaderboard where
    toJSON Leaderboard{..} =
        object [ "count"      .= (show <$> count)
               , "period"     .= (Text.pack . show <$> period)
               , "started_at" .= (Text.pack . Time.formatTimeRFC3339 . (Time.utcToZonedTime Time.utc) <$> startedAt)
               , "userId"     .= (Text.pack . show <$> userId)
               ]

data LeaderboardEntry = LeaderboardEntry { userId    :: Integer
                                         , userLogin :: String
                                         , userName  :: String
                                         , rank      :: Integer
                                         , score     :: Integer
                                         } deriving ( Show, Eq )

instance FromJSON LeaderboardEntry where
    parseJSON = withObject "LeaderboardEntry" $ \o -> do
        uid <- o .: "user_id"
        userLogin <- o .: "user_login"
        userName <- o .: "user_login"
        rank <- o .: "rank"
        score <- o .: "score"
        let userId = read uid :: Integer
        return LeaderboardEntry{..}

data LeaderboardResponse = LeaderboardResponse { endedAt   :: Maybe Time.UTCTime
                                               , startedAt :: Maybe Time.UTCTime
                                               , total     :: Integer
                                               , entries   :: [LeaderboardEntry]
                                               } deriving ( Show, Eq)

instance FromJSON LeaderboardResponse where
    parseJSON = withObject "LeaderboardResponse" $ \o -> do
        dates :: Object <- o .: "date_range"
        ended :: String <- dates .: "ended_at"
        started :: String <- dates .: "started_at"
        entries :: [LeaderboardEntry] <- o .: "data"
        total <- o .: "total"
        let endedAt = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 ended
            startedAt = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 started
        return LeaderboardResponse{..}


cheermotesScope :: String
cheermotesScope = ""

cheermotesRequest :: (String, String)
cheermotesRequest = ("GET", "https://api.twitch.tv/helix/bits/cheermotes")

data Cheermotes = Cheermotes { broadcasterId :: Integer } deriving ( Show, Eq )

instance ToJSON Cheermotes where
    toJSON Cheermotes{..} = object [ "broadcaster_id" .= (show broadcasterId) ]

data CheermoteClass = GlobalFirstParty | GlobalThirdParty | ChannelCustom | DisplayOnly | Sponsored | Unknown deriving ( Eq, Show )

instance Read CheermoteClass where
    readsPrec _ "global_first_party" = [(GlobalFirstParty, "")]
    readsPrec _ "global_third_party" = [(GlobalThirdParty, "")]
    readsPrec _ "channel_custom"     = [(ChannelCustom, "")]
    readsPrec _ "display_only"       = [(DisplayOnly, "")]
    readsPrec _ "sponsored"          = [(Sponsored, "")]
    readsPrec _ _                    = [(Unknown, "")]

data CheermoteData = CheermoteData { tinyURI   :: Maybe String
                                   , smallURI  :: Maybe String
                                   , mediumURI :: Maybe String
                                   , largeURI  :: Maybe String
                                   , hugeURI   :: Maybe String
                                   } deriving ( Eq, Show )

instance FromJSON CheermoteData where
    parseJSON = withObject "CheermoteData" $ \o -> do
        tinyURI   <- o .: "1"
        smallURI  <- o .: "1.5"
        mediumURI <- o .: "2"
        largeURI  <- o .: "3"
        hugeURI   <- o .: "4"
        return CheermoteData{..}

data CheermoteImages = CheermoteImages { darkAnimated  :: CheermoteData
                                       , darkStatic    :: CheermoteData
                                       , lightAnimated :: CheermoteData
                                       , lightStatic   :: CheermoteData
                                       } deriving ( Eq, Show )

instance FromJSON CheermoteImages where
    parseJSON = withObject "CheermoteImages" $ \o -> do
        dark :: Object  <- o .: "dark"
        light :: Object <- o .: "light"
        darkAnimated    <- dark .: "animated"
        darkStatic      <- dark .: "static"
        lightAnimated   <- light .: "animated"
        lightStatic     <- light .: "static"
        return CheermoteImages{..}

data CheermoteTier = CheermoteTier { minBits :: Integer
                                   , cheermoteId :: Integer
                                   , color :: String
                                   , images :: CheermoteImages
                                   , enabled :: Bool
                                   , visible :: Bool
                                   } deriving ( Eq, Show )

instance FromJSON CheermoteTier where
    parseJSON = withObject "CheermoteTier" $ \o -> do
        minBits <- o .: "min_bits"
        cheermoteId <- o .: "id"
        color <- o .: "color"
        images <- o .: "images"
        enabled <- o .: "can_cheer"
        visible <- o .: "show_in_bits_card"
        return CheermoteTier{..}

data CheermotesResponse = CheermotesResponse { prefix :: String
                                             , tiers :: [CheermoteTier]
                                             , cheermoteType :: String
                                             , order :: Integer
                                             , lastUpdated :: Maybe Time.UTCTime
                                             , charitable :: Bool
                                             } deriving ( Eq, Show )

instance FromJSON CheermotesResponse where
    parseJSON = withObject "CheermotesResponse" $ \o -> do
        updated :: String <- o .: "last_updated"
        let lastUpdated = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 updated
        prefix <- o .: "prefix"
        tiers <- o .: "tiers"
        cheermoteType <- o .: "type"
        order <- o .: "order"
        charitable <- o .: "is_charitable"
        return CheermotesResponse{..}
