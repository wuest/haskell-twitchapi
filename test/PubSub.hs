{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Lazy  as BS
import qualified Data.List             as List
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Time             as Time
import qualified Data.Time.RFC3339     as Time ( parseTimeRFC3339 )
import qualified Data.Time.Clock.POSIX as Time ( POSIXTime, posixSecondsToUTCTime )

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

-- Whisper Messages
whisperJSON :: String -> Integer -> String -> Integer -> Integer -> String -> String -> String -> [(Integer, Integer, Integer)] -> Integer -> String -> String -> String -> String
whisperJSON    mid serial msg epoch senderuid senderun senderdisplay sendercolor emote3s recipuid recipun recipdisplay nonce =
    let emotes = "[" ++ (List.intercalate "," $ fmap (\(start, end, emoteId) -> "{\\\"start\\\":" ++ (show start) ++ ",\\\"end\\\":" ++ (show end) ++ ",\\\"id\\\":" ++ (show emoteId) ++"}") emote3s) ++ "]"
        emotes' = "[" ++ (List.intercalate "," $ fmap (\(start, end, emoteId) -> "{\\\\\\\"start\\\\\\\":" ++ (show start) ++ ",\\\\\\\"end\\\\\\\":" ++ (show end) ++ ",\\\\\\\"id\\\\\\\":" ++ (show emoteId) ++"}") emote3s) ++ "]"
    in "{\"type\":\"MESSAGE\",\"data\":{\"topic\":\"whispers.0\",\"message\":\"{\\\"type\\\":\\\"whisper_received\\\",\\\"data\\\":\\\"{\\\\\\\"message_id\\\\\\\":\\\\\\\"" ++ mid ++ "\\\\\\\",\\\\\\\"id\\\\\\\":" ++ (show serial) ++ ",\\\\\\\"thread_id\\\\\\\":\\\\\\\"" ++ ((show senderuid) ++ "_" ++ (show recipuid)) ++ "\\\\\\\",\\\\\\\"body\\\\\\\":\\\\\\\"" ++ msg ++ "\\\\\\\",\\\\\\\"sent_ts\\\\\\\":" ++ (show epoch) ++ ",\\\\\\\"from_id\\\\\\\":" ++ (show senderuid) ++ ",\\\\\\\"tags\\\\\\\":{\\\\\\\"login\\\\\\\":\\\\\\\"" ++ senderun ++ "\\\\\\\",\\\\\\\"display_name\\\\\\\":\\\\\\\"" ++ senderdisplay ++ "\\\\\\\",\\\\\\\"color\\\\\\\":\\\\\\\"#" ++ sendercolor ++ "\\\\\\\",\\\\\\\"emotes\\\\\\\":" ++ emotes' ++ ",\\\\\\\"badges\\\\\\\":[{\\\\\\\"id\\\\\\\":\\\\\\\"notimplementedyetimsorry\\\\\\\",\\\\\\\"version\\\\\\\":\\\\\\\"1\\\\\\\"}]},\\\\\\\"recipient\\\\\\\":{\\\\\\\"id\\\\\\\":" ++ (show recipuid) ++ ",\\\\\\\"username\\\\\\\":\\\\\\\"" ++ recipun ++ "\\\\\\\",\\\\\\\"display_name\\\\\\\":\\\\\\\"" ++ recipdisplay ++ "\\\\\\\",\\\\\\\"color\\\\\\\":\\\\\\\"\\\\\\\"},\\\\\\\"nonce\\\\\\\":\\\\\\\"" ++ nonce ++ "\\\\\\\"}\\\",\\\"data_object\\\":{\\\"message_id\\\":\\\"" ++ mid ++ "\\\",\\\"id\\\":" ++ (show serial) ++ ",\\\"thread_id\\\":\\\"" ++ ((show senderuid) ++ "_" ++ (show recipuid)) ++ "\\\",\\\"body\\\":\\\"" ++ msg ++ "\\\",\\\"sent_ts\\\":" ++ (show epoch) ++ ",\\\"from_id\\\":" ++ (show senderuid) ++ ",\\\"tags\\\":{\\\"login\\\":\\\"" ++ senderun ++ "\\\",\\\"display_name\\\":\\\"" ++ senderdisplay ++ "\\\",\\\"color\\\":\\\"#" ++ sendercolor ++ "\\\",\\\"emotes\\\":" ++ emotes ++ ",\\\"badges\\\":[{\\\"id\\\":\\\"notimplementedyetimsorry\\\",\\\"version\\\":\\\"1\\\"}]},\\\"recipient\\\":{\\\"id\\\":" ++ (show recipuid) ++ ",\\\"username\\\":\\\"" ++ recipun ++ "\\\",\\\"display_name\\\":\\\"" ++ recipdisplay ++ "\\\",\\\"color\\\":\\\"\\\"},\\\"nonce\\\":\\\"" ++ nonce ++ "\\\"}}\"}}"

main :: IO ()
main = hspec $ describe "PubSub interactions" $ do
    it "Creates SuccessResponses" $ property prop_successResponse
    it "Creates ErrorResponses" $ property prop_errorResponse
    it "Parses Bits (v2) Messages" $ property prop_errorResponse
    it "Parses Whisper Messages" $ property prop_whisperMessage

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

prop_whisperMessage :: AlphaNumericString -> Integer -> AlphaNumericString -> Integer -> Integer -> AlphaNumericString -> AlphaNumericString -> AlphaNumericString -> [(Integer, Integer, Integer)] -> Integer -> AlphaNumericString -> AlphaNumericString -> NumericString -> Bool
prop_whisperMessage mid' serial msg' epoch' senderuid senderun' senderdisplay' sendercolor' emote3s recipuid recipun' recipdisplay' nonce' =
    let mid = unwrapAlphaNumeric mid'
        msg = unwrapAlphaNumeric msg'
        epoch = abs epoch'
        senderun = unwrapAlphaNumeric senderun'
        senderdisplay = unwrapAlphaNumeric senderdisplay'
        sendercolor = unwrapAlphaNumeric sendercolor'
        recipun = unwrapAlphaNumeric recipun'
        recipdisplay = unwrapAlphaNumeric recipdisplay'
        nonce = unwrapNumeric nonce'
        threadId = ((show senderuid) ++ "_" ++ (show recipuid))
        time = Just . Time.posixSecondsToUTCTime $ realToFrac $ epoch
        emotes = fmap (\(s, e, i) -> Web.TwitchAPI.PubSub.EmoteSpec s e i) emote3s
        senderInfo = Web.TwitchAPI.PubSub.UserInfo senderuid senderun (Just senderdisplay)
        recipInfo = Web.TwitchAPI.PubSub.UserInfo recipuid recipun (Just recipdisplay)
        json = whisperJSON mid serial msg epoch senderuid senderun senderdisplay sendercolor emote3s recipuid recipun recipdisplay nonce
        response = Right $ Web.TwitchAPI.PubSub.WhisperMessage mid threadId time msg emotes senderInfo ("#" ++ sendercolor) recipInfo
        parsed = parseJSON json :: Either String Web.TwitchAPI.PubSub.Message
    in response == parsed
