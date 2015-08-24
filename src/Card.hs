{-# LANGUAGE OverloadedStrings #-}

module Card where

import Snap.Core
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.ByteString
import Data.UUID.V4
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.Maybe

data Card = Card { token :: ByteString
                 , front :: ByteString
                 , back :: ByteString
                 } deriving (Show)

instance ToRow Card where
  toRow c = [ toField (token c)
            , toField (front c)
            , toField (back c)
            ]

insertCard :: Query
insertCard = "INSERT INTO cards (token, front, back) VALUES (?, ?, ?)"

cardsResource :: Connection -> Snap ()
cardsResource conn = method POST (postToCards conn) <|> pass

postToCards :: Connection -> Snap ()
postToCards conn = do maybeCard <- cardToCreate
                      maybe
                        (do modifyResponse $ setResponseStatus 400 "bad request"
                            r <- getResponse
                            finishWith r)
                        (createCard conn)
                        maybeCard

{-
 - CREATE CARD
 -}
createCard :: Connection -> Card -> Snap ()
createCard conn card = do inserted <- liftIO $ execute conn insertCard card
                          unless (inserted == 1) $
                            do modifyResponse $ setResponseStatus 500 "server error"
                               r <- getResponse
                               finishWith r

cardToCreate :: Snap (Maybe Card)
cardToCreate = do reqToken <- getQueryParam "token"
                  reqFront <- getQueryParam "front"
                  reqBack <- getQueryParam "back"
                  return (reqToken >>= \t ->
                         (reqFront >>= \f ->
                         (reqBack >>= \b ->
                         Just $ Card t f b)))

