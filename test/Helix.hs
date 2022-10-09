{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import qualified Helix.Users

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = Helix.Users.main
