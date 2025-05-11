{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.TwitchAPI.Helix.EventSub where

import Prelude

import qualified Data.Time             as Time
import qualified Network.HTTP.Client   as HTTP

import Control.Monad ( mzero )
import Data.Aeson    ( ToJSON(..), encode, toJSON
                     , FromJSON(..), (.:), withObject, withText
                     )
import Data.Text     ( Text )

import qualified Web.TwitchAPI.Helix.Request    as Req
import qualified Web.TwitchAPI.EventSub.Request as ESReq

data SubscriptionRequest = SubscriptionRequest ESReq.Transport ESReq.Subscription

instance Req.HelixRequest SubscriptionRequest where
    toRequest (SubscriptionRequest t s) =
        let setBody r = r{ HTTP.requestBody = HTTP.RequestBodyLBS . encode . toJSON $ ESReq.toRequest t s }
        in setBody $ HTTP.parseRequest_ "POST https://api.twitch.tv/helix/eventsub/subscriptions"
    scope _ = Nothing
    -- TODO: This probably needs reworking right?  Scope per entry?

data Status = Enabled
            | CallbackVerificationPending
            deriving ( Show, Eq )
instance FromJSON Status where
    parseJSON = withText "status" $ \case
        "enabled" -> return Enabled
        "webhook_callback_verification_pending" -> return CallbackVerificationPending
        _ -> mzero

type Callback = Text
type Secret = Text
type SessionID = Text
type ConduitID = Text
data Transport = Webhook !Callback
               | Websocket !SessionID !Time.UTCTime
               | Conduit !ConduitID
               deriving ( Show, Eq )
instance FromJSON Transport where
    parseJSON = withObject "Transport" $ \o -> do
        method :: Text <- o .: "method"
        case method of
            "webhook" -> do
                callback <- o .: "callback"
                return $ Webhook callback
            "websocket" -> do
                sessionID <- o .: "session_id"
                connectedAt <- o .: "connected_at"
                return $ Websocket sessionID connectedAt
            "conduit" -> do
                conduitID <- o .: "conduit_id"
                return $ Conduit conduitID
            _ -> mzero

data SubscriptionResponse = SubscriptionResponse { createdID :: Text
                                                 , createdStatus :: Status
                                                 , createdType :: Text -- TODO: bad
                                                 , createdAt :: Time.UTCTime
                                                 , createdTransport :: Transport
                                                 , createdCost :: Integer
                                                 , createdTotal :: Integer
                                                 , createdTotalCost :: Integer
                                                 , createdMaxTotalCost :: Integer
                                                 } deriving ( Show, Eq )
instance FromJSON SubscriptionResponse where
    parseJSON = withObject "SubscriptionResponse" $ \o -> do
        datum <- o .: "data"
        case datum of
            x:_ -> do
                createdID <- x .: "id"
                createdStatus <- x .: "status"
                createdType <- x .: "type"
                createdAt <- x .: "created_at"
                createdTransport <- x .: "transport"
                createdCost <- x .: "cost"
                createdTotal <- o .: "total"
                createdTotalCost <- o .: "total_cost"
                createdMaxTotalCost <- o .: "max_total_cost"
                return SubscriptionResponse{..}
            [] -> mzero
