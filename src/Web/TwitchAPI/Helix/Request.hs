{-# LANGUAGE DuplicateRecordFields #-}

module Web.TwitchAPI.Helix.Request where

import Prelude

import qualified Data.ByteString as BS
import qualified Data.Aeson      as JSON

data HelixRequest = Get { endpoint :: String
                        , queryParameters :: [(BS.ByteString, Maybe BS.ByteString)]
                        }
                  | Post { endpoint :: String
                         , queryParameters :: [(BS.ByteString, Maybe BS.ByteString)]
                         , requestBody :: JSON.Value
                         } deriving ( Show )
