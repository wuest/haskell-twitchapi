{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.TwitchAPI.Helix.Users where

import Prelude

import Data.Functor  ( (<&>) )

import qualified Data.ByteString.Char8 as BS
import qualified Data.Time             as Time
import qualified Data.Time.RFC3339     as Time ( parseTimeRFC3339 )
import qualified Network.HTTP.Client   as HTTP

import Data.Aeson        ( FromJSON(..), (.:), withObject )
import Data.Aeson.KeyMap ( toAscList )

import qualified Web.TwitchAPI.Helix.Request as Req

-- BUG: Not yet implemented:
--   - Update User
--   - Block/Unblock User
--   - Update User Extensions

data User = User { lookupID :: Maybe String
                 , username :: Maybe String
                 } deriving ( Show )

instance Req.HelixRequest User where
    toRequest user =
        let lookupID' :: [(BS.ByteString, Maybe BS.ByteString)] = maybe [] (\i -> [("id", Just . BS.pack . show $ i)]) (lookupID user)
            username' :: [(BS.ByteString, Maybe BS.ByteString)] = maybe [] (\u -> [("login", Just . BS.pack $ u)]) (username user)
            setQuery = HTTP.setQueryString $ lookupID' ++ username'
        in setQuery $ HTTP.parseRequest_ "GET https://api.twitch.tv/helix/users"
    scope User{} = Just "user:read:email"

data BroadcasterType = Partner | Affiliate | None deriving ( Eq, Show )

instance Read BroadcasterType where
    readsPrec _ "partner"   = [(Partner, "")]
    readsPrec _ "affiliate" = [(Affiliate, "")]
    readsPrec _ _           = [(None, "")]

data UserType = Staff | Admin | GlobalMod | NormalUser deriving ( Eq, Show )

instance Read UserType where
    readsPrec _ "staff"      = [(Staff, "")]
    readsPrec _ "admin"      = [(Admin, "")]
    readsPrec _ "global_mod" = [(GlobalMod, "")]
    readsPrec _ _            = [(NormalUser, "")]

data UserEntry = UserEntry { broadcasterType :: BroadcasterType
                           , description     :: String
                           , displayName     :: String
                           , userId          :: Integer
                           , login           :: String
                           , offlineImageURL :: String
                           , profileImageURL :: String
                           , userType        :: UserType
                           , email           :: Maybe String
                           , createdAt       :: Maybe Time.UTCTime
                           } deriving ( Show, Eq )

instance FromJSON UserEntry where
    parseJSON = withObject "UserEntry" $ \o -> do
        userId' :: String <- o .: "id"
        created :: String <- o .: "created_at"
        userType' :: String <- o .: "type"
        broadcasterType' :: String <- o .: "broadcaster_type"
        let userId = read userId' :: Integer
            createdAt = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 created
            userType = read userType' :: UserType
            broadcasterType = read broadcasterType' :: BroadcasterType

        description <- o .: "description"
        displayName <- o .: "display_name"
        login <- o .: "login"
        offlineImageURL <- o .: "offline_image_url"
        profileImageURL <- o .: "profile_image_url"
        email <- o .: "email"
        return UserEntry{..}

data Users = Users { lookupIDs :: [Integer]
                   , usernames :: [String]
                   } deriving ( Show, Eq )

instance Req.HelixRequest Users where
    toRequest users =
        let lookupID' :: [(BS.ByteString, Maybe BS.ByteString)] = (\i -> ("id", Just . BS.pack . show $ i)) <$> lookupIDs users
            username' :: [(BS.ByteString, Maybe BS.ByteString)] = (\u -> ("login", Just . BS.pack $ u)) <$> usernames users
            setQuery = HTTP.setQueryString $ lookupID' ++ username'
        in setQuery $ HTTP.parseRequest_ "GET https://api.twitch.tv/helix/users"
    scope Users{} = Nothing

newtype UsersResponse = UsersResponse { users :: [UserEntry] } deriving ( Eq, Show )

instance FromJSON UsersResponse where
    parseJSON = withObject "UsersResponse" $ \o -> do
        users <- o .: "data"
        return UsersResponse{..}

data Follows = Follows { after :: Maybe String
                       , max :: Maybe Integer
                       , from :: Maybe Integer
                       , to :: Maybe Integer
                       } deriving ( Show, Eq )

instance Req.HelixRequest Follows where
    toRequest user =
        let after' :: [(BS.ByteString, Maybe BS.ByteString)] = maybe [] (\a -> [("after", Just . BS.pack $ a)]) (after user)
            max' :: [(BS.ByteString, Maybe BS.ByteString)] = maybe [] (\u -> [("first", Just . BS.pack . show $ u)]) (from user)
            fromID' :: [(BS.ByteString, Maybe BS.ByteString)] = maybe [] (\u -> [("after", Just . BS.pack . show $ u)]) (from user)
            toID' :: [(BS.ByteString, Maybe BS.ByteString)] = maybe [] (\u -> [("after", Just . BS.pack . show $ u)]) (to user)
            setQuery = HTTP.setQueryString $ concat [after', max', fromID', toID']
        in setQuery $ HTTP.parseRequest_ "GET https://api.twitch.tv/helix/users/follows"
    scope Follows{} = Nothing

data FollowEntry = FollowEntry { fromID :: Integer
                               , fromLogin :: String
                               , fromName :: String
                               , toID :: Integer
                               , toName :: String
                               , followedAt :: Maybe Time.UTCTime
                               } deriving ( Show, Eq )

instance FromJSON FollowEntry where
    parseJSON = withObject "FollowEntry" $ \o -> do
        fromID' :: String <- o .: "from_id"
        toID' :: String <- o .: "to_id"
        followedAt' :: String <- o .: "followed_at"
        let fromID = read fromID' :: Integer
            toID = read toID' :: Integer
            followedAt = Time.zonedTimeToUTC <$> Time.parseTimeRFC3339 followedAt'
        fromLogin <- o .: "from_login"
        fromName <- o .: "from_name"
        toName <- o .: "to_name"
        return FollowEntry{..}

data FollowsResponse = FollowsResponse { total :: Integer
                                       , users :: [FollowEntry]
                                       , paginationCursor :: String
                                       } deriving ( Show, Eq )

instance FromJSON FollowsResponse where
    parseJSON = withObject "FollowsResponse" $ \o -> do
        total <- o .: "total"
        paginationCursor <- (o .: "pagination") >>= (.: "cursor")
        users <- o .: "data"
        return FollowsResponse{..}

newtype BlockList = BlockList { broadcasterID :: Integer } deriving ( Show, Eq )

instance Req.HelixRequest BlockList where
    toRequest user =
        HTTP.setQueryString [("login", Just . BS.pack . show . broadcasterID $ user)] $
            HTTP.parseRequest_ "https://api.twitch.tv/helix/users/blocks"
    scope BlockList{} = Just "user:read:blocked_users"

data BlockListEntry = BlockListEntry { userId :: Integer
                                     , userLogin :: String
                                     , displayName :: String
                                     } deriving ( Show, Eq )

instance FromJSON BlockListEntry where
    parseJSON = withObject "BlockListEntry" $ \o -> do
        userId' :: String <- o .: "user_id"
        let userId = read userId' :: Integer
        userLogin <- o .: "user_login"
        displayName <- o .: "display_name"
        return BlockListEntry{..}

newtype BlockListResponse = BlockListResponse { blocks :: [BlockListEntry] } deriving ( Show, Eq )

instance FromJSON BlockListResponse where
    parseJSON = withObject "BlockListResponse" $ \o -> do
        blocks <- o .: "data"
        return BlockListResponse{..}

data Extensions = Extensions

instance Req.HelixRequest Extensions where
    toRequest _ = HTTP.parseRequest_ "GET https://api.twitch.tv/helix/users/extensions/list"
    scope Extensions = Just "user:read:broadcast"

data ExtensionType = Component | Mobile | Panel | Overlay deriving ( Show, Eq )

instance Read ExtensionType where
    readsPrec _ "component" = [(Component, "")]
    readsPrec _ "mobile"    = [(Mobile, "")]
    readsPrec _ "panel"     = [(Panel, "")]
    readsPrec _ "overlay"   = [(Overlay, "")]
    readsPrec _ _           = mempty

data ExtensionsEntry = ExtensionsEntry { canActivate :: Bool
                                       , extensionId :: String
                                       , name :: String
                                       , extensionTypes :: [ExtensionType]
                                       , version :: String
                                       } deriving ( Show, Eq )

instance FromJSON ExtensionsEntry where
    parseJSON = withObject "ExtensionsEntry" $ \o -> do
        extensionTypes' :: [String] <- o .: "type"
        let extensionTypes :: [ExtensionType] = read <$> extensionTypes'
        canActivate <- o .: "can_activate"
        extensionId <- o .: "id"
        name <- o .: "name"
        version <- o .: "version"
        return ExtensionsEntry{..}

newtype ExtensionsResponse = ExtensionsResponse { extensions :: [ExtensionsEntry] } deriving ( Show, Eq )

instance FromJSON ExtensionsResponse where
    parseJSON = withObject "ExtensionsResponse" $ \o -> do
        extensions <- o .: "data"
        return ExtensionsResponse{..}

newtype ActiveExtensions = ActiveExtensions { userID :: Maybe Integer } deriving ( Show, Eq )

instance Req.HelixRequest ActiveExtensions where
    toRequest user =
        let userID' :: [(BS.ByteString, Maybe BS.ByteString)] = maybe [] (\i -> [("user_id", Just . BS.pack . show $ i)]) (userID user)
            setQuery = HTTP.setQueryString userID'
        in setQuery $ HTTP.parseRequest_ "GET https://api.twitch.tv/helix/users/extensions"
    scope ActiveExtensions{} = Nothing

data ActiveComponentExtensionEntry' = ActiveComponentExtensionEntry' { active :: Bool
                                                                     , extensionId :: String
                                                                     , version :: String
                                                                     , name :: String
                                                                     , x :: Integer
                                                                     , y :: Integer
                                                                     }
                                    | InactiveComponentExtension deriving ( Show, Eq )

instance FromJSON ActiveComponentExtensionEntry' where
    parseJSON = withObject "ActiveExtensionEntry" $ \o -> do
        active <- o .: "active"
        if active then do
            extensionId <- o .: "id"
            version <- o .: "version"
            name <- o .: "name"
            x <- o .: "x"
            y <- o .: "y"
            return ActiveComponentExtensionEntry'{..}
        else return InactiveComponentExtension

data ActiveComponentExtensionEntry = ActiveComponentExtensionEntry { active :: Bool
                                                                   , extensionId :: String
                                                                   , version :: String
                                                                   , name :: String
                                                                   , x :: Integer
                                                                   , y :: Integer
                                                                   } deriving ( Show, Eq )

filterActiveComponentExtensions :: [ActiveComponentExtensionEntry'] -> [ActiveComponentExtensionEntry]
filterActiveComponentExtensions = reverse . foldl filterActiveComponentExtensions' []

filterActiveComponentExtensions' :: [ActiveComponentExtensionEntry] -> ActiveComponentExtensionEntry' -> [ActiveComponentExtensionEntry]
filterActiveComponentExtensions' as InactiveComponentExtension = as
filterActiveComponentExtensions' as (ActiveComponentExtensionEntry' _ i v n x y) = ActiveComponentExtensionEntry True i v n x y : as

data ActiveExtensionEntry' = ActiveExtensionEntry' { active :: Bool
                                                   , extensionId :: String
                                                   , version :: String
                                                   , name :: String
                                                   } 
                          | InactiveExtension deriving ( Show, Eq )

instance FromJSON ActiveExtensionEntry' where
    parseJSON = withObject "ActiveExtensionEntry" $ \o -> do
        active <- o .: "active"
        if active then do
            extensionId <- o .: "id"
            version <- o .: "version"
            name <- o .: "name"
            return ActiveExtensionEntry'{..}
        else return InactiveExtension

data ActiveExtensionEntry = ActiveExtensionEntry { active :: Bool
                                                 , extensionId :: String
                                                 , version :: String
                                                 , name :: String
                                                 } deriving ( Show, Eq )

filterActiveExtensions :: [ActiveExtensionEntry'] -> [ActiveExtensionEntry]
filterActiveExtensions = reverse . foldl filterActiveExtensions' []

filterActiveExtensions' :: [ActiveExtensionEntry] -> ActiveExtensionEntry' -> [ActiveExtensionEntry]
filterActiveExtensions' as InactiveExtension = as
filterActiveExtensions' as (ActiveExtensionEntry' _ i v n) = ActiveExtensionEntry True i v n : as

data ActiveExtensionsResponse = ActiveExtensionsResponse { components :: [ActiveComponentExtensionEntry]
                                                         , overlays :: [ActiveExtensionEntry]
                                                         , panels :: [ActiveExtensionEntry]
                                                         } deriving ( Show, Eq )

instance FromJSON ActiveExtensionsResponse where
    parseJSON = withObject "ActiveExtensionsResponse" $ \o -> do
        components <- (((o .: "data") >>= (.: "component")) <&> ((snd <$>) . toAscList)) <&> filterActiveComponentExtensions
        overlays <- (((o .: "data") >>= (.: "overlay")) <&> ((snd <$>) . toAscList)) <&> filterActiveExtensions
        panels <- (((o .: "data") >>= (.: "panel")) <&> ((snd <$>) . toAscList)) <&> filterActiveExtensions
        return ActiveExtensionsResponse{..}
