{-# LANGUAGE DuplicateRecordFields #-}

module Web.TwitchAPI.Helix.Request where

import Prelude

import qualified Network.HTTP.Client as HTTP

class HelixRequest a where
    toRequest :: a -> HTTP.Request
    scope :: a -> Maybe String
