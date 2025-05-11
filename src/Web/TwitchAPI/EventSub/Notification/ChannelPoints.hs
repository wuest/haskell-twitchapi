{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.ChannelPoints
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to channel points (redemption, reward management)
-}

module Web.TwitchAPI.EventSub.Notification.ChannelPoints where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:), (.:?)
                     , Object, withObject, withText
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor )

import qualified Web.TwitchAPI.EventSub.Notification.Emote as Emote

data RewardMessage = RewardMessage { rewardText :: Text
                                   , rewardEmotes :: [Emote.Emote]
                                   } deriving ( Show, Eq )
instance FromJSON RewardMessage where
    parseJSON = withObject "RewardMessage" $ \o -> do
        rewardText <- o .: "text"
        rewardEmotes <- o .: "emotes"
        return RewardMessage{..}

data Emote = Emote { emoteID :: Text
                   , emoteName :: Text
                   } deriving ( Show, Eq )
instance FromJSON Emote where
    parseJSON = withObject "Emote" $ \o -> do
        emoteID <- o .: "id"
        emoteName <- o .: "name"
        return Emote{..}

data RewardInfo = SingleMessageBypassSubMode
                | HighlightedMessage
                | RandomSubEmoteUnlock Emote
                | ChosenSubEmoteUnlock Emote
                | ChosenModifiedSubEmoteUnlock Emote
                | MessageEffect
                | GigantifyEmote
                | Celebration
                deriving ( Show, Eq )
instance FromJSON RewardInfo where
    parseJSON = withObject "Reward" $ \o -> do
        rewardType :: Text <- o .: "type"
        case rewardType of
            "single_message_bypass_sub_mode" -> return SingleMessageBypassSubMode
            "send_highlighted_message" -> return HighlightedMessage
            "random_sub_emote_unlock" -> do
                unlockedEmote <- o .: "unlocked_emote"
                return $ RandomSubEmoteUnlock unlockedEmote
            "chosen_sub_emote_unlock" -> do
                unlockedEmote <- o .: "unlocked_emote"
                return $ ChosenSubEmoteUnlock unlockedEmote
            "chosen_modified_sub_emote_unlock" -> do
                unlockedEmote <- o .: "unlocked_emote"
                return $ ChosenModifiedSubEmoteUnlock unlockedEmote
            "message_effect" -> return MessageEffect
            "gigantify_an_emote" -> return GigantifyEmote
            "celebration" -> return Celebration
            _ -> mzero

data Image = Image { url1x :: Text
                   , url2x :: Text
                   , url4x :: Text
                   } deriving ( Show, Eq )
instance FromJSON Image where
    parseJSON = withObject "Image" $ \o -> do
        url1x <- o .: "url_1x"
        url2x <- o .: "url_2x"
        url4x <- o .: "url_4x"
        return Image{..}

data Status = Unknown
            | Unfulfilled
            | Fulfilled
            | Canceled
            deriving ( Show, Eq )
instance FromJSON Status where
    parseJSON = withText "Status" $ \case
        "unknown" -> return Unknown
        "unfulfilled" -> return Unfulfilled
        "fulfilled" -> return Fulfilled
        "canceled" -> return Canceled
        _ -> mzero

data Reward = Reward { rewardID :: Text
                     , rewardTitle :: Text
                     , rewardCost :: Integer
                     , rewardPrompt :: Text
                     } deriving ( Show, Eq )
instance FromJSON Reward where
    parseJSON = withObject "Reward" $ \o -> do
        rewardID <- o .: "id"
        rewardTitle <- o .: "title"
        rewardCost <- o .: "cost"
        rewardPrompt <- o .: "prompt"
        return Reward{..}

data Message = AutomaticRewardRedemptionAddV1 { broadcaster :: User
                                              , user :: User
                                              , redemptionID :: Text
                                              , rewardInfo :: RewardInfo
                                              , message :: RewardMessage
                                              , userInput :: Maybe Text
                                              , redeemedAt :: Time.UTCTime
                                              }
             | AutomaticRewardRedemptionAdd { broadcaster :: User
                                            , user :: User
                                            , redemptionID :: Text
                                            , rewardInfo :: RewardInfo
                                            , message :: RewardMessage
                                            , userInput :: Maybe Text
                                            , redeemedAt :: Time.UTCTime
                                            }
             | CustomRewardAdd { customRewardID :: Text
                               , broadcaster :: User
                               , enabled :: Bool
                               , paused :: Bool
                               , stocked :: Bool
                               , title :: Text
                               , cost :: Integer
                               , prompt :: Text
                               , inputRequired :: Bool
                               , skipQueue :: Bool
                               , streamMax :: Maybe Integer
                               , userMax :: Maybe Integer
                               , backgroundColor :: Text
                               , image :: Image
                               , defaultImage :: Image
                               , globalCooldown :: Maybe Integer
                               , cooldownExpiration :: Maybe Time.UTCTime
                               , redemptionCount :: Maybe Integer
                               }
             | CustomRewardUpdate { customRewardID :: Text
                                  , broadcaster :: User
                                  , enabled :: Bool
                                  , paused :: Bool
                                  , stocked :: Bool
                                  , title :: Text
                                  , cost :: Integer
                                  , prompt :: Text
                                  , inputRequired :: Bool
                                  , skipQueue :: Bool
                                  , streamMax :: Maybe Integer
                                  , userMax :: Maybe Integer
                                  , backgroundColor :: Text
                                  , image :: Image
                                  , defaultImage :: Image
                                  , globalCooldown :: Maybe Integer
                                  , cooldownExpiration :: Maybe Time.UTCTime
                                  , redemptionCount :: Maybe Integer
                                  }
             | CustomRewardRemove { customRewardID :: Text
                                  , broadcaster :: User
                                  , enabled :: Bool
                                  , paused :: Bool
                                  , stocked :: Bool
                                  , title :: Text
                                  , cost :: Integer
                                  , prompt :: Text
                                  , inputRequired :: Bool
                                  , skipQueue :: Bool
                                  , streamMax :: Maybe Integer
                                  , userMax :: Maybe Integer
                                  , backgroundColor :: Text
                                  , image :: Image
                                  , defaultImage :: Image
                                  , globalCooldown :: Maybe Integer
                                  , cooldownExpiration :: Maybe Time.UTCTime
                                  , redemptionCount :: Maybe Integer
                                  }
             | CustomRewardRedemptionAdd { redemptionID :: Text
                                         , broadcaster :: User
                                         , user :: User
                                         , input :: Text
                                         , status :: Status
                                         , reward :: Reward
                                         , redeemedAt :: Time.UTCTime
                                         }
             | CustomRewardRedemptionUpdate { redemptionID :: Text
                                            , broadcaster :: User
                                            , user :: User
                                            , input :: Text
                                            , status :: Status
                                            , reward :: Reward
                                            , redeemedAt :: Time.UTCTime
                                            }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

automaticRewardRedemptionAddV1 :: MessageParser
automaticRewardRedemptionAddV1 o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    redemptionID <- o .: "id"
    rewardInfo <- o .: "reward"
    message <- o .: "message"
    userInput <- o .:? "user_input"
    redeemedAt <- o .: "redeemed_at"
    return AutomaticRewardRedemptionAddV1{..}

automaticRewardRedemptionAdd :: MessageParser
automaticRewardRedemptionAdd o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    redemptionID <- o .: "id"
    rewardInfo <- o .: "reward"
    message <- o .: "message"
    userInput <- o .:? "user_input"
    redeemedAt <- o .: "redeemed_at"
    return AutomaticRewardRedemptionAdd{..}

customRewardAdd :: MessageParser
customRewardAdd o = do
    customRewardID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    enabled <- o .: "is_enabled"
    paused <- o .: "is_paused"
    stocked <- o .: "is_in_stock"
    title <- o .: "title"
    cost <- o .: "cost"
    prompt <- o .: "prompt"
    inputRequired <- o .: "is_user_input_required"
    skipQueue <- o .: "should_redemptions_skip_request_queue"
    backgroundColor <- o .: "background_color"
    image <- o .: "image"
    defaultImage <- o .: "default_image"

    streamMax' <- o .: "max_per_stream"
    streamMaxEnabled <- streamMax' .: "is_enabled"
    streamMax <- if streamMaxEnabled then streamMax' .:? "value"
                                     else return Nothing
    userMax' <- o .: "max_per_stream"
    userMaxEnabled <- userMax' .: "is_enabled"
    userMax <- if userMaxEnabled then userMax' .:? "value"
                                 else return Nothing
    globalCooldown' <- o .: "global_cooldown"
    globalCooldownEnabled <- globalCooldown' .: "is_enabled"
    globalCooldown <- if globalCooldownEnabled then globalCooldown' .:? "value"
                                               else return Nothing

    cooldownExpiration <- o .:? "cooldown_expires_at"
    redemptionCount <- o .:? "redemptions_redeemed_current_stream"
    return CustomRewardAdd{..}

customRewardUpdate :: MessageParser
customRewardUpdate o = do
    customRewardID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    enabled <- o .: "is_enabled"
    paused <- o .: "is_paused"
    stocked <- o .: "is_in_stock"
    title <- o .: "title"
    cost <- o .: "cost"
    prompt <- o .: "prompt"
    inputRequired <- o .: "is_user_input_required"
    skipQueue <- o .: "should_redemptions_skip_request_queue"
    backgroundColor <- o .: "background_color"
    image <- o .: "image"
    defaultImage <- o .: "default_image"

    streamMax' <- o .: "max_per_stream"
    streamMaxEnabled <- streamMax' .: "is_enabled"
    streamMax <- if streamMaxEnabled then streamMax' .:? "value"
                                     else return Nothing
    userMax' <- o .: "max_per_stream"
    userMaxEnabled <- userMax' .: "is_enabled"
    userMax <- if userMaxEnabled then userMax' .:? "value"
                                 else return Nothing
    globalCooldown' <- o .: "global_cooldown"
    globalCooldownEnabled <- globalCooldown' .: "is_enabled"
    globalCooldown <- if globalCooldownEnabled then globalCooldown' .:? "value"
                                               else return Nothing

    cooldownExpiration <- o .:? "cooldown_expires_at"
    redemptionCount <- o .:? "redemptions_redeemed_current_stream"
    return CustomRewardUpdate{..}

customRewardRemove :: MessageParser
customRewardRemove o = do
    customRewardID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    enabled <- o .: "is_enabled"
    paused <- o .: "is_paused"
    stocked <- o .: "is_in_stock"
    title <- o .: "title"
    cost <- o .: "cost"
    prompt <- o .: "prompt"
    inputRequired <- o .: "is_user_input_required"
    skipQueue <- o .: "should_redemptions_skip_request_queue"
    backgroundColor <- o .: "background_color"
    image <- o .: "image"
    defaultImage <- o .: "default_image"

    streamMax' <- o .: "max_per_stream"
    streamMaxEnabled <- streamMax' .: "is_enabled"
    streamMax <- if streamMaxEnabled then streamMax' .:? "value"
                                     else return Nothing
    userMax' <- o .: "max_per_stream"
    userMaxEnabled <- userMax' .: "is_enabled"
    userMax <- if userMaxEnabled then userMax' .:? "value"
                                 else return Nothing
    globalCooldown' <- o .: "global_cooldown"
    globalCooldownEnabled <- globalCooldown' .: "is_enabled"
    globalCooldown <- if globalCooldownEnabled then globalCooldown' .:? "value"
                                               else return Nothing

    cooldownExpiration <- o .:? "cooldown_expires_at"
    redemptionCount <- o .:? "redemptions_redeemed_current_stream"
    return CustomRewardRemove{..}

customRewardRedemptionAdd :: MessageParser
customRewardRedemptionAdd o = do
    redemptionID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    input <- o .: "user_input"
    status <- o .: "status"
    reward <- o .: "reward"
    redeemedAt <- o .: "redeemed_at"
    return CustomRewardRedemptionAdd{..}

customRewardRedemptionUpdate :: MessageParser
customRewardRedemptionUpdate o = do
    redemptionID <- o .: "id"
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    input <- o .: "user_input"
    status <- o .: "status"
    reward <- o .: "reward"
    redeemedAt <- o .: "redeemed_at"
    return CustomRewardRedemptionUpdate{..}
