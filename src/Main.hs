{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Data.ByteString
import Snap.Core
import Snap.Http.Server
import Snap.Http.Server.Config
import Database.PostgreSQL.Simple
import Control.Applicative
import Card

main :: IO ()
main = do conn <- testConnect
          httpServe config (app conn)

app :: Connection -> Snap ()
app conn = path "cards" (cardsResource conn) <|> pass

ok :: Response
ok = setResponseCode 200 emptyResponse

config :: Config Snap a
config = setHostname "localhost" $ setPort 1234 mempty

testConnect :: IO Connection
testConnect = connectPostgreSQL "host=localhost port=5432 dbname=locus_test"

devConnect :: IO Connection
devConnect = connectPostgreSQL "host=localhost port=5432 dbname=locus_dev"
