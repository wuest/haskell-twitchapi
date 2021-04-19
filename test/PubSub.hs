{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Time            as Time
import qualified Data.Time.RFC3339    as Time ( parseTimeRFC3339 )

import qualified Web.TwitchAPI.PubSub

import Data.Char   ( isDigit, isLetter )
import Text.Printf ( printf )

import Test.QuickCheck
import Test.Hspec

newtype AlphaString = AlphaString { unwrapAlpha :: String } deriving Show
instance Arbitrary AlphaString where
    arbitrary = AlphaString <$> (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'])

newtype AlphaNumericString = AlphaNumericString { unwrapAlphaNumeric :: String } deriving Show
instance Arbitrary AlphaNumericString where
    arbitrary = AlphaNumericString <$> (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

newtype NumericString = NumericString { unwrapNumeric :: String } deriving Show
instance Arbitrary NumericString where
    arbitrary = NumericString <$> (listOf1 $ elements ['0'..'9'])

newtype RFC3339 = RFC3339 { unwrapDate :: String } deriving Show
instance Arbitrary RFC3339 where
    arbitrary = do
        year' :: Integer <- arbitrary
        month' :: Integer <- arbitrary
        day' :: Integer <- arbitrary
        hour' :: Integer <- arbitrary
        minute' :: Integer <- arbitrary
        second' :: Integer <- arbitrary
        fractional' :: Integer <- arbitrary
        let year = (year' `mod` 10) + 2010
            month = (month' `mod` 12) + 1
            day = (day' `mod` 28) + 1
            hour = hour' `mod` 24
            minute = minute' `mod` 60
            second = second' `mod` 60
            fractional = fractional' `mod` 1000000000
            dateString :: String = printf "%4d-%02d-%02dT%02d:%02d:%02d%09dZ" year month day hour minute second fractional
        return $ RFC3339 dateString

parseJSON :: JSON.FromJSON a => String -> Either String a
parseJSON x = JSON.eitherDecode $ (BS.fromStrict . T.encodeUtf8 . T.pack) x

-- Server Responses

successResponseJSON :: String -> String
successResponseJSON n = "{\"type\":\"RESPONSE\",\"error\":\"\",\"nonce\":\"" ++ n ++ "\"}"

errorResponseJSON :: String -> String -> String
errorResponseJSON n e = "{\"type\":\"RESPONSE\",\"error\":\"" ++ e ++ "\",\"nonce\":\"" ++ n ++ "\"}"

-- Bits Messages

bitsV2JSON :: String -> String -> String -> String -> String -> String -> Integer -> Integer -> String -> Maybe (Integer, Integer) -> String
bitsV2JSON u c uid cid time msg bits total mid Nothing = "{\"type\":\"MESSAGE\",\"data\":{\"topic\":\"channel-bits-events-v2.0\",\"message\":\"{\\\"data\\\":{\\\"user_name\\\":\\\"" ++ u ++ "\\\",\\\"channel_name\\\":\\\"" ++ c ++ "\\\",\\\"user_id\\\":\\\"" ++ uid ++ "\\\",\\\"channel_id\\\":\\\"" ++ cid ++ "\\\",\\\"time\\\":\\\"" ++ time ++ "\\\",\\\"chat_message\\\":\\\"" ++ msg ++ "\\\",\\\"bits_used\\\":" ++ (show bits) ++ ",\\\"total_bits_used\\\":" ++ (show total) ++ ",\\\"is_anonymous\\\":false,\\\"context\\\":\\\"cheer\\\",\\\"badge_entitlement\\\":null},\\\"version\\\":\\\"1.0\\\",\\\"message_type\\\":\\\"bits_event\\\",\\\"message_id\\\":\\\"" ++ mid ++ "\\\"}\"}}"
bitsV2JSON u c uid cid time msg bits total mid (Just (nb, pb)) = "{\"type\":\"MESSAGE\",\"data\":{\"topic\":\"channel-bits-events-v2.0\",\"message\":\"{\\\"data\\\":{\\\"user_name\\\":\\\"" ++ u ++ "\\\",\\\"channel_name\\\":\\\"" ++ c ++ "\\\",\\\"user_id\\\":\\\"" ++ uid ++ "\\\",\\\"channel_id\\\":\\\"" ++ cid ++ "\\\",\\\"time\\\":\\\"" ++ time ++ "\\\",\\\"chat_message\\\":\\\"" ++ msg ++ "\\\",\\\"bits_used\\\":" ++ (show bits) ++ ",\\\"total_bits_used\\\":" ++ (show total) ++ ",\\\"is_anonymous\\\":false,\\\"context\\\":\\\"cheer\\\",\\\"badge_entitlement\\\":{\\\"new_version\\\":" ++ (show nb) ++ ", \\\"previous_version\\\":" ++ (show pb) ++ "}},\\\"version\\\":\\\"1.0\\\",\\\"message_type\\\":\\\"bits_event\\\",\\\"message_id\\\":\\\"" ++ mid ++ "\\\"}\"}}"

main :: IO ()
main = hspec $ describe "PubSub interactions" $ do
    it "Creates SuccessResponses" $ property prop_successResponse
    it "Creates ErrorResponses" $ property prop_errorResponse
    it "Parses Bits (v2) Messages" $ property prop_errorResponse

prop_successResponse :: AlphaNumericString -> Bool
prop_successResponse n =
    let nonce = unwrapAlphaNumeric n
        json = successResponseJSON nonce
        response = Right $ Web.TwitchAPI.PubSub.SuccessMessage (Just nonce)
        parsed = parseJSON json :: Either String Web.TwitchAPI.PubSub.Message
    in response == parsed

prop_errorResponse :: AlphaNumericString -> AlphaString -> Bool
prop_errorResponse n e =
    let nonce = unwrapAlphaNumeric n
        err = unwrapAlpha e
        json = errorResponseJSON nonce err
        response = Right $ Web.TwitchAPI.PubSub.ErrorMessage (Just nonce) err
        parsed = parseJSON json :: Either String Web.TwitchAPI.PubSub.Message
    in response == parsed

prop_bitsV2Message :: AlphaNumericString -> AlphaNumericString -> NumericString -> NumericString -> RFC3339 -> AlphaNumericString -> Integer -> Integer -> AlphaNumericString -> Maybe (Integer, Integer) -> Bool
prop_bitsV2Message un' cn' uid' cid' time' msg' bits total mid' unlock' =
    let un = unwrapAlphaNumeric un'
        cn = unwrapAlphaNumeric cn'
        uid = unwrapNumeric uid'
        cid = unwrapNumeric cid'
        time = unwrapDate time'
        utcTime = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 time
        msg = unwrapAlphaNumeric msg'
        mid = unwrapAlphaNumeric mid'
        context = "cheer"
        messageType = "bits_event"
        version = "1.0"
        unlock = fmap (\(n,p) -> Web.TwitchAPI.PubSub.BadgeUnlock n p) unlock'
        json = bitsV2JSON un cn uid cid time msg bits total mid unlock'
        response = Right $ Web.TwitchAPI.PubSub.BitsV2Message unlock bits (read cid :: Integer) (Just msg) context mid messageType utcTime total (Just (read uid :: Integer)) (Just un) version
        parsed = parseJSON json :: Either String Web.TwitchAPI.PubSub.Message
    in response == parsed
