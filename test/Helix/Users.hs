{-# LANGUAGE ScopedTypeVariables #-}

module Helix.Users ( main ) where

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Lazy  as BS
import qualified Data.List             as List
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Time             as Time
import qualified Data.Time.RFC3339     as Time ( parseTimeRFC3339 )
import qualified Data.Time.Clock.POSIX as Time ( POSIXTime, posixSecondsToUTCTime )

import qualified Web.TwitchAPI.Helix.Users as Users

import Data.Char   ( isDigit, isLetter )
import Text.Printf ( printf )

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = do
    hspec $ describe "Users Interactions (Twitch Fixtures)" $ do
        it "Parses Twitch's Single-User JSON Fixture" $ property prop_userJSONFixture
        it "Parses Twitch's Multi-User JSON Fixture" $ property prop_usersJSONFixture
        it "Parses Twitch's Follow JSON Fixture" $ property prop_followsJSONFixture
        it "Parses Twitch's Block List JSON Fixture" $ property prop_blocksJSONFixture
        it "Parses Twitch's Extensions List JSON Fixture" $ property prop_extensionsJSONFixture
        it "Parses Twitch's Active Extensions List JSON Fixture" $ property prop_activeExtensionsJSONFixture

prop_userJSONFixture :: Bool
prop_userJSONFixture =
    let broadcasterType = Users.Partner
        description = "Supporting third-party developers building Twitch integrations from chatbots to game integrations."
        displayName = "TwitchDev"
        userId = 141981764
        login = "twitchdev"
        offlineImageURL = "https://static-cdn.jtvnw.net/jtv_user_pictures/3f13ab61-ec78-4fe6-8481-8682cb3b0ac2-channel_offline_image-1920x1080.png"
        profileImageURL = "https://static-cdn.jtvnw.net/jtv_user_pictures/8a6381c7-d0c0-4576-b179-38bd5ce1d6af-profile_image-300x300.png"
        userType = Users.NormalUser
        email = Just "not-real@email.com"
        createdAt = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 "2016-12-14T20:32:28Z"
        record = Users.UserEntry broadcasterType description displayName userId login offlineImageURL profileImageURL userType email createdAt
        response = Right $ Users.UsersResponse [record]
        parsed = parseJSON singleUserJSON :: Either String Users.UsersResponse
    in response == parsed

prop_usersJSONFixture :: Bool
prop_usersJSONFixture =
    let broadcasterType = Users.Partner
        description = "Supporting third-party developers building Twitch integrations from chatbots to game integrations."
        displayName = "TwitchDev"
        userId = 141981764
        login = "twitchdev"
        offlineImageURL = "https://static-cdn.jtvnw.net/jtv_user_pictures/3f13ab61-ec78-4fe6-8481-8682cb3b0ac2-channel_offline_image-1920x1080.png"
        profileImageURL = "https://static-cdn.jtvnw.net/jtv_user_pictures/8a6381c7-d0c0-4576-b179-38bd5ce1d6af-profile_image-300x300.png"
        userType = Users.NormalUser
        email = Just "not-real@email.com"
        createdAt = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 "2016-12-14T20:32:28Z"
        record = Users.UserEntry broadcasterType description displayName userId login offlineImageURL profileImageURL userType email createdAt
        response = Right $ Users.UsersResponse [record, record]
        parsed = parseJSON multiUserJSON :: Either String Users.UsersResponse
    in response == parsed

prop_followsJSONFixture :: Bool
prop_followsJSONFixture =
    let a_from_id = 171003792
        a_from_login = "iiisutha067iii"
        a_from_name = "IIIsutha067III"
        a_to_id = 23161357
        a_to_name = "LIRIK"
        a_followed = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 "2017-08-22T22:55:24Z"

        b_from_id = 113627897
        b_from_login = "birdman616"
        b_from_name = "Birdman616"
        b_to_id = 23161357
        b_to_name = "LIRIK"
        b_followed = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 "2017-08-22T22:55:04Z"

        a = Users.FollowEntry a_from_id a_from_login a_from_name a_to_id a_to_name a_followed
        b = Users.FollowEntry b_from_id b_from_login b_from_name b_to_id b_to_name b_followed
        total = 12345
        cursor = "eyJiIjpudWxsLCJhIjoiMTUwMzQ0MTc3NjQyNDQyMjAwMCJ9"
        response = Right $ Users.FollowsResponse total [a, b] cursor
        parsed = parseJSON followsJSON :: Either String Users.FollowsResponse
    in response == parsed

prop_blocksJSONFixture :: Bool
prop_blocksJSONFixture =
    let a_user_id = 135093069
        a_user_login = "bluelava"
        a_display_name = "BlueLava"

        b_user_id = 27419011
        b_user_login = "travistyoj"
        b_display_name = "TravistyOJ"

        a = Users.BlockListEntry a_user_id a_user_login a_display_name
        b = Users.BlockListEntry b_user_id b_user_login b_display_name
        response = Right $ Users.BlockListResponse [a, b]
        parsed = parseJSON blocksJSON :: Either String Users.BlockListResponse
    in response == parsed

prop_extensionsJSONFixture :: Bool
prop_extensionsJSONFixture =
    let a_i = "wi08ebtatdc7oj83wtl9uxwz807l8b"
        a_v = "1.1.8"
        a_n = "Streamlabs Leaderboard"
        a_c = True
        a_t = [Users.Panel]

        b_i = "d4uvtfdr04uq6raoenvj7m86gdk16v"
        b_v = "2.0.2"
        b_n = "Prime Subscription and Loot Reminder"
        b_c = True
        b_t = [Users.Overlay]


        c_i = "rh6jq1q334hqc2rr1qlzqbvwlfl3x0"
        c_v = "1.1.0"
        c_n = "TopClip"
        c_c = True
        c_t = [Users.Mobile, Users.Panel]

        d_i = "zfh2irvx2jb4s60f02jq0ajm8vwgka"
        d_v = "1.0.19"
        d_n = "Streamlabs"
        d_c = True
        d_t = [Users.Mobile, Users.Overlay]

        e_i = "lqnf3zxk0rv0g7gq92mtmnirjz2cjj"
        e_v = "0.0.1"
        e_n = "Dev Experience Test"
        e_c = True
        e_t = [Users.Component, Users.Mobile, Users.Panel, Users.Overlay]

        a = Users.ExtensionsEntry a_c a_i a_n a_t a_v
        b = Users.ExtensionsEntry b_c b_i b_n b_t b_v
        c = Users.ExtensionsEntry c_c c_i c_n c_t c_v
        d = Users.ExtensionsEntry d_c d_i d_n d_t d_v
        e = Users.ExtensionsEntry e_c e_i e_n e_t e_v
        response = Right $ Users.ExtensionsResponse [a, b, c, d, e]
        parsed = parseJSON extensionsJSON :: Either String Users.ExtensionsResponse
    in response == parsed

prop_activeExtensionsJSONFixture :: Bool
prop_activeExtensionsJSONFixture =
    let p_1_i = "rh6jq1q334hqc2rr1qlzqbvwlfl3x0"
        p_1_v = "1.1.0"
        p_1_n = "TopClip"

        p_2_i = "wi08ebtatdc7oj83wtl9uxwz807l8b"
        p_2_v = "1.1.8"
        p_2_n = "Streamlabs Leaderboard"

        p_3_i = "naty2zwfp7vecaivuve8ef1hohh6bo"
        p_3_v = "1.0.9"
        p_3_n = "Streamlabs Stream Schedule & Countdown"

        o_i = "zfh2irvx2jb4s60f02jq0ajm8vwgka"
        o_v = "1.0.19"
        o_n = "Streamlabs"

        c_i = "lqnf3zxk0rv0g7gq92mtmnirjz2cjj"
        c_v = "0.0.1"
        c_n = "Dev Experience Test"
        c_x = 0
        c_y = 0

        p = [ Users.ActiveExtensionEntry True p_1_i p_1_v p_1_n
            , Users.ActiveExtensionEntry True p_2_i p_2_v p_2_n
            , Users.ActiveExtensionEntry True p_3_i p_3_v p_3_n
            ]
        o = [ Users.ActiveExtensionEntry True o_i o_v o_n ]
        c = [ Users.ActiveComponentExtensionEntry True c_i c_v c_n c_x c_y ]
        response = Right $ Users.ActiveExtensionsResponse c o p
        parsed = parseJSON activeExtensionsJSON :: Either String Users.ActiveExtensionsResponse
    in response == parsed

parseJSON :: JSON.FromJSON a => String -> Either String a
parseJSON x = JSON.eitherDecode $ (BS.fromStrict . T.encodeUtf8 . T.pack) x

-- Server Responses

singleUserJSON :: String
singleUserJSON = "{\"data\": [{\"id\": \"141981764\",\"login\": \"twitchdev\",\"display_name\": \"TwitchDev\",\"type\": \"\",\"broadcaster_type\": \"partner\",\"description\": \"Supporting third-party developers building Twitch integrations from chatbots to game integrations.\",\"profile_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/8a6381c7-d0c0-4576-b179-38bd5ce1d6af-profile_image-300x300.png\",\"offline_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/3f13ab61-ec78-4fe6-8481-8682cb3b0ac2-channel_offline_image-1920x1080.png\",\"view_count\": 5980557,\"email\": \"not-real@email.com\",\"created_at\": \"2016-12-14T20:32:28Z\"}]}"

multiUserJSON :: String
multiUserJSON = "{\"data\": [{\"id\": \"141981764\",\"login\": \"twitchdev\",\"display_name\": \"TwitchDev\",\"type\": \"\",\"broadcaster_type\": \"partner\",\"description\": \"Supporting third-party developers building Twitch integrations from chatbots to game integrations.\",\"profile_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/8a6381c7-d0c0-4576-b179-38bd5ce1d6af-profile_image-300x300.png\",\"offline_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/3f13ab61-ec78-4fe6-8481-8682cb3b0ac2-channel_offline_image-1920x1080.png\",\"view_count\": 5980557,\"email\": \"not-real@email.com\",\"created_at\": \"2016-12-14T20:32:28Z\"}, {\"id\": \"141981764\",\"login\": \"twitchdev\",\"display_name\": \"TwitchDev\",\"type\": \"\",\"broadcaster_type\": \"partner\",\"description\": \"Supporting third-party developers building Twitch integrations from chatbots to game integrations.\",\"profile_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/8a6381c7-d0c0-4576-b179-38bd5ce1d6af-profile_image-300x300.png\",\"offline_image_url\": \"https://static-cdn.jtvnw.net/jtv_user_pictures/3f13ab61-ec78-4fe6-8481-8682cb3b0ac2-channel_offline_image-1920x1080.png\",\"view_count\": 5980557,\"email\": \"not-real@email.com\",\"created_at\": \"2016-12-14T20:32:28Z\"}]}"

followsJSON :: String
followsJSON = "{\"total\": 12345,\"data\":[{\"from_id\": \"171003792\",\"from_login\": \"iiisutha067iii\",\"from_name\": \"IIIsutha067III\",\"to_id\": \"23161357\",\"to_name\": \"LIRIK\",\"followed_at\": \"2017-08-22T22:55:24Z\"},{\"from_id\": \"113627897\",\"from_login\": \"birdman616\",\"from_name\": \"Birdman616\",\"to_id\": \"23161357\",\"to_name\": \"LIRIK\",\"followed_at\": \"2017-08-22T22:55:04Z\"}],\"pagination\":{\"cursor\": \"eyJiIjpudWxsLCJhIjoiMTUwMzQ0MTc3NjQyNDQyMjAwMCJ9\"}}"

blocksJSON :: String
blocksJSON = "{\"data\": [{\"user_id\": \"135093069\",\"user_login\": \"bluelava\",\"display_name\": \"BlueLava\"},{\"user_id\": \"27419011\",\"user_login\": \"travistyoj\",\"display_name\": \"TravistyOJ\"}]}"

extensionsJSON :: String
extensionsJSON = "{\"data\": [{\"id\": \"wi08ebtatdc7oj83wtl9uxwz807l8b\",\"version\": \"1.1.8\",\"name\": \"Streamlabs Leaderboard\",\"can_activate\": true,\"type\": [\"panel\"]},{\"id\": \"d4uvtfdr04uq6raoenvj7m86gdk16v\",\"version\": \"2.0.2\",\"name\": \"Prime Subscription and Loot Reminder\",\"can_activate\": true,\"type\": [\"overlay\"]},{\"id\": \"rh6jq1q334hqc2rr1qlzqbvwlfl3x0\",\"version\": \"1.1.0\",\"name\": \"TopClip\",\"can_activate\": true,\"type\": [\"mobile\",\"panel\"]},{\"id\": \"zfh2irvx2jb4s60f02jq0ajm8vwgka\",\"version\": \"1.0.19\",\"name\": \"Streamlabs\",\"can_activate\": true,\"type\": [\"mobile\",\"overlay\"]},{\"id\": \"lqnf3zxk0rv0g7gq92mtmnirjz2cjj\",\"version\": \"0.0.1\",\"name\": \"Dev Experience Test\",\"can_activate\": true,\"type\": [\"component\",\"mobile\",\"panel\",\"overlay\"]}]}"

activeExtensionsJSON :: String
activeExtensionsJSON = "{\"data\": {\"panel\": {\"1\": {\"active\": true,\"id\": \"rh6jq1q334hqc2rr1qlzqbvwlfl3x0\",\"version\": \"1.1.0\",\"name\": \"TopClip\"},\"2\": {\"active\": true,\"id\": \"wi08ebtatdc7oj83wtl9uxwz807l8b\",\"version\": \"1.1.8\",\"name\": \"Streamlabs Leaderboard\"},\"3\": {\"active\": true,\"id\": \"naty2zwfp7vecaivuve8ef1hohh6bo\",\"version\": \"1.0.9\",\"name\": \"Streamlabs Stream Schedule & Countdown\"}},\"overlay\": {\"1\": {\"active\": true,\"id\": \"zfh2irvx2jb4s60f02jq0ajm8vwgka\",\"version\": \"1.0.19\",\"name\": \"Streamlabs\"}},\"component\": {\"1\": {\"active\": true,\"id\": \"lqnf3zxk0rv0g7gq92mtmnirjz2cjj\",\"version\": \"0.0.1\",\"name\": \"Dev Experience Test\",\"x\": 0,\"y\": 0},\"2\": {\"active\": false}}}}"
