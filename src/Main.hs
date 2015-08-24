{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Data.ByteString
import Snap.Core
import Snap.Http.Server
import Snap.Http.Server.Config
import Database.PostgreSQL.Simple

main :: IO ()
main = do conn <- testConnect
          httpServe config app

app :: Snap ()
app = putResponse ok

ok :: Response
ok = setResponseCode 200 emptyResponse

config :: Config Snap a
config = setHostname "localhost" $ setPort 1234 mempty

testConnect :: IO Connection
testConnect = connectPostgreSQL "host=localhost port=5432 dbname=locus_test"

devConnect :: IO Connection
devConnect = connectPostgreSQL "host=localhost port=5432 dbname=locus_dev"


