{-# LANGUAGE OverloadedStrings #-}

module Card where

import Prelude hiding (concat)
import Snap.Core
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.ByteString
import Data.UUID.V4
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception
import Data.Maybe
import SnapUtil

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
cardsResource conn = method POST (postToCards conn)

postToCards :: Connection -> Snap ()
postToCards conn = cardFromRequest >>= saveCard conn

cardFromRequest :: Snap Card
cardFromRequest = do t <- getSingleParam "token"
                     f <- getSingleParam "front"
                     b <- getSingleParam "back"
                     return $ Card t f b

saveCard :: Connection -> Card -> Snap ()
saveCard conn card = do result <- liftIO (tryJust isUniqueConstraintViolation $
                            execute conn insertCard card)
                        case result of
                             Left (SqlError _ _ msg detail _) ->
                                finishWithError (setResponseStatus 409 (concat [msg, "\n", detail]))
                             Right _ -> return ()

isUniqueConstraintViolation :: SqlError -> Maybe SqlError
isUniqueConstraintViolation e = case e of
                                     SqlError "23505" _ _ _ _ -> Just e
                                     _ -> Nothing
