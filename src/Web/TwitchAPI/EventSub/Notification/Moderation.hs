{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Notification.Moderation
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Functions relating to moderation (ban/unban, moderator management, warnings)
-}

module Web.TwitchAPI.EventSub.Notification.Moderation where

import Prelude

import Control.Monad ( mzero )
import Data.Aeson    ( FromJSON(..), (.:), (.:?)
                     , Object, withObject, withText
                     )
import Data.Text     ( Text )

import qualified Data.Aeson.Types as JSON.Types
import qualified Data.Time        as Time

import Web.TwitchAPI.EventSub.Notification.User ( User, userFor, userFor' )

import qualified Web.TwitchAPI.EventSub.Notification.Message as Message

data UnbanRequestStatus = Approved
                        | Canceled
                        | Denied
                        deriving ( Show, Eq )
instance FromJSON UnbanRequestStatus where
    parseJSON = withText "UnbanRequestStatus" $ \case
        "approved" -> return Approved
        "denied" -> return Denied
        "canceled" -> return Canceled
        _ -> mzero

data Suspicion = Manual
               | BanEvader
               | BannedInSharedChannel
               deriving ( Show, Eq )
instance FromJSON Suspicion where
    parseJSON = withText "SusicionTypes" $ \case
        "manually_added" -> return Manual
        "ban_evader" -> return BanEvader
        "banned_in_shared_channel" -> return BannedInSharedChannel
        _ -> mzero

data Likelihood = Unknown
                | Possible
                | Likely
                deriving ( Show, Eq )
instance FromJSON Likelihood where
    parseJSON = withText "BanEvasionEvaluation" $ \case
        "unknown" -> return Unknown
        "possible" -> return Possible
        "likely" -> return Likely
        _ -> mzero

data Trust = NoStatus
           | ActiveMonitoring
           | Restricted
           deriving ( Show, Eq )
instance FromJSON Trust where
    parseJSON = withText "LowTrustStatus" $ \case
        "none" -> return NoStatus
        "active_monitoring" -> return ActiveMonitoring
        "restricted" -> return Restricted
        _ -> mzero

data Action = Ban { target :: User
                  , modComment :: Maybe Text
                  }
            | Unban { target :: User }
            | Timeout { target :: User
                      , modComment :: Maybe Text
                      , expires :: Time.UTCTime
                      }
            | Untimeout { target :: User }
            | Clear
            | EmoteOnly
            | EmoteOnlyOff
            | Followers { minutes :: Integer }
            | FollowersOff
            | UniqueChat
            | UniqueChatOff
            | Slow { seconds :: Integer }
            | SlowOff
            | Subscribers
            | SubscribersOff
            | Raid { target :: User
                   , count :: Integer
                   }
            | Unraid { target :: User }
            | Delete { target :: User
                     , messageID :: Text
                     , messageBody :: Text
                     }
            | VIP { target :: User }
            | UnVIP { target :: User }
            | AddBlocked { terms :: [Text]
                         , automod :: Bool
                         }
            | RemoveBlocked { terms :: [Text]
                            , automod :: Bool
                            }
            | AddPermitted { terms :: [Text]
                           , automod :: Bool
                           }
            | RemovePermitted { terms :: [Text]
                              , automod :: Bool
                              }
            | Mod { target :: User }
            | UnMod { target :: User }
            | ApproveUnbanRequest { target :: User
                                  , moderatorMessage :: Text
                                  }
            | DenyUnbanRequest { target :: User
                               , moderatorMessage :: Text
                               }
            | Warn { target :: User
                   , warning :: Text
                   , rulesCited :: [Text]
                   }
            | SharedChatBan { target :: User
                            , modComment :: Maybe Text
                            }
            | SharedChatTimeout { target :: User
                                , modComment :: Maybe Text
                                , expires :: Time.UTCTime
                                }
            | SharedChatUntimeout { target :: User }
            | SharedChatUnban { target :: User }
            | SharedChatDelete { target :: User
                               , messageID :: Text
                               , messageBody :: Text
                               }
            deriving ( Show, Eq )
instance FromJSON Action where
    parseJSON = withObject "Action" $ \action -> do
        actionName :: Text <- action .: "action"
        case actionName of
            "ban" -> do
                o <- action .: "ban"
                target <- userFor "" o
                modComment <- o .:? "reason"
                return Ban{..}
            "shared_chat_ban" -> do
                o <- action .: "shared_chat_ban"
                target <- userFor "" o
                modComment <- o .:? "reason"
                return SharedChatBan{..}
            "timeout" -> do
                o <- action .: "timeout"
                target <- userFor "" o
                modComment <- o .:? "reason"
                expires <- o .: "expires_at"
                return Timeout{..}
            "shared_chat_timeout" -> do
                o <- action .: "shared_chat_timeout"
                target <- userFor "" o
                modComment <- o .:? "reason"
                expires <- o .: "expires_at"
                return SharedChatTimeout{..}
            "unban" -> do
                o <- action .: "unban"
                target <- userFor "" o
                return Unban{..}
            "shared_chat_unban" -> do
                o <- action .: "shared_chat_unban"
                target <- userFor "" o
                return SharedChatUnban{..}
            "untimeout" -> do
                o <- action .: "untimeout"
                target <- userFor "" o
                return Untimeout{..}
            "shared_chat_untimeout" -> do
                o <- action .: "shared_chat_untimeout"
                target <- userFor "" o
                return SharedChatUntimeout{..}
            "clear" -> return Clear
            "emoteonly" -> return EmoteOnly
            "emoteonlyoff" -> return EmoteOnlyOff
            "followers" -> do
                o <- action .: "followers"
                minutes <- o .: "follow_duration_minutes"
                return Followers{..}
            "followersoff" -> return FollowersOff
            "uniquechat" -> return UniqueChat
            "uniquechatoff" -> return UniqueChatOff
            "slow" -> do
                o <- action .: "slow"
                seconds <- o .: "wait_time_seconds"
                return Slow{..}
            "slowoff" -> return SlowOff
            "subscribers" -> return Subscribers
            "subscribersoff" -> return SubscribersOff
            "unraid" -> do
                o <- action .: "unraid"
                target <- userFor "" o
                return Unraid{..}
            "delete" -> do
                o <- action .: "delete"
                target <- userFor "" o
                messageID <- o .: "message_id"
                messageBody <- o .: "message_body"
                return Delete{..}
            "shared_chat_delete" -> do
                o <- action .: "shared_chat_delete"
                target <- userFor "" o
                messageID <- o .: "message_id"
                messageBody <- o .: "message_body"
                return SharedChatDelete{..}
            "unvip" -> do
                o <- action .: "unvip"
                target <- userFor "" o
                return UnVIP{..}
            "vip" -> do
                o <- action .: "vip"
                target <- userFor "" o
                return VIP{..}
            "raid" -> do
                o <- action .: "raid"
                target <- userFor "" o
                count <- o .: "viewer_count"
                return Raid{..}
            "add_blocked_term" -> do
                o <- action .: "automod_terms"
                terms <- o .: "terms"
                automod <- o .: "from_automod"
                return AddBlocked{..}
            "add_permitted_term" -> do
                o <- action .: "automod_terms"
                terms <- o .: "terms"
                automod <- o .: "from_automod"
                return AddPermitted{..}
            "remove_blocked_term" -> do
                o <- action .: "automod_terms"
                terms <- o .: "terms"
                automod <- o .: "from_automod"
                return RemoveBlocked{..}
            "remove_permitted_term" -> do
                o <- action .: "automod_terms"
                terms <- o .: "terms"
                automod <- o .: "from_automod"
                return RemovePermitted{..}
            "mod" -> do
                o <- action .: "mod"
                target <- userFor "" o
                return Mod{..}
            "unmod" -> do
                o <- action .: "unmod"
                target <- userFor "" o
                return UnMod{..}
            "approve_unban_request" -> do
                o <- action .: "unban_request"
                target <- userFor "" o
                moderatorMessage <- o .: "moderator_message"
                return ApproveUnbanRequest{..}
            "deny_unban_request" -> do
                o <- action .: "unban_request"
                target <- userFor "" o
                moderatorMessage <- o .: "moderator_message"
                return DenyUnbanRequest{..}
            "warn" -> do
                o <- action .: "warn"
                target <- userFor "" o
                warning <- o .: "reason"
                rulesCited <- o .: "chat_rules_cited"
                return Warn{..}
            _ -> mzero

data Message = BanUser { broadcaster :: User
                       , user :: User
                       , moderator :: User
                       , banReason :: Text
                       , time :: Time.UTCTime
                       , endTime :: Maybe Time.UTCTime
                       }
             | UnbanUser { broadcaster :: User
                         , user :: User
                         , moderator :: User
                         }
             | UnbanRequestCreate { broadcaster :: User
                                  , user :: User
                                  , requestID :: Text
                                  , body :: Text
                                  , time :: Time.UTCTime
                                  }
             | UnbanRequestResolve { broadcaster :: User
                                   , user :: User
                                   , moderator :: User
                                   , requestID :: Text
                                   , body :: Text
                                   , status :: UnbanRequestStatus
                                   }
             | ModerateV1 { broadcaster :: User
                          , sourceBroadcaster :: Maybe User
                          , moderator :: User
                          , action :: Action
                          }
             | Moderate { broadcaster :: User
                        , sourceBroadcaster :: Maybe User
                        , moderator :: User
                        , action :: Action
                        }
             | ModeratorAdd { broadcaster :: User
                            , user :: User
                            }
             | ModeratorRemove { broadcaster :: User
                               , user :: User
                               }
             | SuspiciousUserMessage { broadcaster :: User
                                     , user :: User
                                     , trust :: Trust
                                     , types :: [Suspicion]
                                     , evasionEvaluation :: Likelihood
                                     , message :: Message.Message
                                     }
             | SuspiciousUserUpdate { broadcaster :: User
                                    , moderator :: User
                                    , user :: User
                                    , trust :: Trust
                                    }
             | WarningAcknowledgement { broadcaster :: User
                                      , user :: User
                                      }
             | WarningSend { broadcaster :: User
                           , moderator :: User
                           , user :: User
                           , reason :: Maybe Text
                           , rules :: [Text]
                           }
             deriving ( Show, Eq )

type MessageParser = Object -> JSON.Types.Parser Message

banUser :: MessageParser
banUser o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    moderator <- userFor "moderator" o
    time <- o .: "banned_at"
    banReason <- o .: "reason"
    endTime <- o .:? "end_time"
    return BanUser{..}

unbanUser :: MessageParser
unbanUser o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    moderator <- userFor "moderator" o
    return UnbanUser{..}

unbanRequestCreate :: MessageParser
unbanRequestCreate o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    requestID <- o .: "id"
    body <- o .: "text"
    time <- o .: "created_at"
    return UnbanRequestCreate{..}

unbanRequestResolve :: MessageParser
unbanRequestResolve o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    moderator <- userFor "moderator" o
    requestID <- o .: "id"
    body <- o .: "resolution_text"
    status <- o .: "status"
    return UnbanRequestResolve{..}

moderateV1 :: MessageParser
moderateV1 o = do
    broadcaster <- userFor "broadcaster" o
    sourceBroadcaster <- userFor' "source_broadcaster" o
    moderator <- userFor "moderator" o
    action <- o .: "action"
    return ModerateV1{..}

moderate :: MessageParser
moderate o = do
    broadcaster <- userFor "broadcaster" o
    sourceBroadcaster <- userFor' "source_broadcaster" o
    moderator <- userFor "moderator" o
    action <- o .: "action"
    return Moderate{..}

moderatorAdd :: MessageParser
moderatorAdd o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    return ModeratorAdd{..}

moderatorRemove :: MessageParser
moderatorRemove o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    return ModeratorRemove{..}

suspiciousUserMessage :: MessageParser
suspiciousUserMessage o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    trust <- o .: "trust"
    types <- o .: "types"
    evasionEvaluation <- o .: "ban_evasion_evaluation"
    message <- o .: "message"
    return SuspiciousUserMessage{..}

suspiciousUserUpdate :: MessageParser
suspiciousUserUpdate o = do
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor "moderator" o
    user <- userFor "" o
    trust <- o .: "trust"
    return SuspiciousUserUpdate{..}

warningAckowledgement :: MessageParser
warningAckowledgement o = do
    broadcaster <- userFor "broadcaster" o
    user <- userFor "" o
    return WarningAcknowledgement{..}

warningSend :: MessageParser
warningSend o = do
    broadcaster <- userFor "broadcaster" o
    moderator <- userFor "moderator" o
    user <- userFor "" o
    reason <- o .:? "reason"
    rules' <- o .:? "chat_rules_cited"
    let rules = case rules' of Nothing   -> []
                               (Just rs) -> rs
    return WarningSend{..}
