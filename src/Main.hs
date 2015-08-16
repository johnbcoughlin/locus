{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Snap.Core
import Snap.Http.Server
import Snap.Http.Server.Config

main :: IO ()
main = httpServe config $ app

app :: Snap ()
app = putResponse ok

ok :: Response
ok = setResponseCode 200 emptyResponse

config :: Config Snap a
config = setHostname "localhost" $ setPort 1234 mempty
