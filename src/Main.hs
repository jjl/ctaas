{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import           Data.ByteString
import           Data.ByteString.Lazy as L (toStrict)
-- import qualified Data.ByteString.Char8 as C8
import           Control.Applicative
import qualified Control.Concurrent.Map as M
import           Control.Monad.IO.Class
import           Snap.Core
import           Snap.Http.Server

type StringMap = M.Map ByteString ByteString

-- self-explanatory name. 64MB ought to be big enough for now
max_body_size = 1024 * 1024 * 64

-- Entry

main :: IO ()
main = (M.empty :: IO StringMap) >>= \m -> quickHttpServe $ site m

-- Handlers

site m = getRequest >>= \r -> (handler (rqURI r) m) <|> invalidRequest

handler k m = getKeyHandler k m <|> postKeyHandler k m <|>
                delKeyHandler k m <|> invalidRequest

getKeyHandler k m = get' $ getKey k m
delKeyHandler k m = del' $ delKey k m
postKeyHandler k m = post' $ readRequestBody max_body_size >>=
                             \b -> putKey k (toStrict b) m

-- Wrappers over the ctrie

getKey k m = (liftIO $ M.lookup k m) >>= \v ->maybe (notFound k) writeBS v
delKey k m = (liftIO $ M.delete k m) >> noContent
putKey k v m = (liftIO $ M.insert k v m) >> created

-- Helpers

get' a = method GET a
post' a = method POST a
del' a = method DELETE a

notFound a = status 404 >> (writeBS $ append "Not Found: " a)
invalidRequest = status 400 >> writeBS "Invalid Request"
noContent = status 204 >> writeBS "No Content"
created = status 201 >> writeBS "Created"
status = modifyResponse . setResponseCode
