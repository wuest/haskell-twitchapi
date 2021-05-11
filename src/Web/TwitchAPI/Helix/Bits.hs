{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.TwitchAPI.Helix.Bits where

import Prelude

import qualified Data.ByteString     as BS
import qualified Data.Time           as Time
import qualified Data.Time.RFC3339   as Time ( formatTimeRFC3339, parseTimeRFC3339 )
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text ( encodeUtf8 )
import qualified Network.HTTP.Client as HTTP

import Data.Aeson ( FromJSON(..), (.:), withObject
                  , ToJSON(..), (.=), object
                  , Object
                  )
import Data.Maybe ( fromMaybe )

leaderboardScope :: String
leaderboardScope = "bits:read"

leaderboardRequest :: LeaderboardRequest -> HTTP.Request
leaderboardRequest params =
    let req = HTTP.parseRequest_ "GET https://api.twitch.tv/helix/bits/leaderboard"
        count'     :: [(BS.ByteString, Maybe BS.ByteString)] = fromMaybe [] $ (\c -> ("count", Just . Text.encodeUtf8 . Text.pack . show $ c):[]) <$> (count params)
        period'    :: [(BS.ByteString, Maybe BS.ByteString)] = fromMaybe [] $ (\p -> ("period", Just . Text.encodeUtf8 . Text.pack . show $ p):[]) <$> (period params)
        startedAt' :: [(BS.ByteString, Maybe BS.ByteString)] = fromMaybe [] $ (\s -> ("started_at", Just . Text.encodeUtf8 $ (Time.formatTimeRFC3339 $ Time.utcToZonedTime Time.utc s :: Text.Text)):[]) <$> (startedAt (params :: LeaderboardRequest))
        userId'    :: [(BS.ByteString, Maybe BS.ByteString)] = fromMaybe [] $ (\u -> ("user_id", Just . Text.encodeUtf8 . Text.pack . show $ u):[]) <$> (userId (params :: LeaderboardRequest))
        query = foldl (++) [] [count', period', startedAt', userId']
    in  HTTP.setQueryString query req

data Period = Day | Week | Month | Year | All deriving ( Eq )
instance Show Period where
    show Day   = "day"
    show Week  = "week"
    show Month = "month"
    show Year  = "year"
    show All   = "all"

data LeaderboardRequest = LeaderboardRequest { count     :: Maybe Integer
                                             , period    :: Maybe Period
                                             , startedAt :: Maybe Time.UTCTime
                                             , userId    :: Maybe Integer
                                             } deriving ( Show, Eq )

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

cheermotesRequest :: CheermotesRequest -> HTTP.Request
cheermotesRequest params =
    let req = HTTP.parseRequest_ "GET https://api.twitch.tv/helix/bits/cheermotes"
        broadcasterId' :: [(BS.ByteString, Maybe BS.ByteString)] = ("broadcaster_id", Just . Text.encodeUtf8 . Text.pack . show $ broadcasterId params):[]
    in  HTTP.setQueryString broadcasterId' req

data CheermotesRequest = CheermotesRequest { broadcasterId :: Integer } deriving ( Show, Eq )

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
