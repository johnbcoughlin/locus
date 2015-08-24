{-# LANGUAGE OverloadedStrings #-}

module SnapUtil where

import Snap.Core
import Data.Maybe
import Data.ByteString hiding (head)
import Control.Monad

finishWithError :: MonadSnap m => (Response -> Response) -> m a
finishWithError error = modifyResponse error >> (getResponse >>= finishWith)

getSingleParam :: ByteString -> Snap ByteString
getSingleParam name = do req <- getRequest
                         maybe (finishWithError $ paramNotFound name)
                           (\vals -> if Prelude.length vals /= 1
                                        then finishWithError $ duplicateParam name
                                        else return $ head vals)
                           (rqParam name req)

paramNotFound :: ByteString -> Response -> Response
paramNotFound name = setResponseStatus 400 (append "param not found: " name)

duplicateParam :: ByteString -> Response -> Response
duplicateParam name = setResponseStatus 400 (append "duplicate param values: " name)
