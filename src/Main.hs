{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.ByteString
import           Data.ByteString.Lazy as L (toStrict)
-- import qualified Data.ByteString.Char8 as C8
import           Data.Hashable
import           Control.Applicative
import qualified Control.Concurrent.Map as M
import           Control.Monad.IO.Class
import           GHC.Generics (Generic)
import           Snap.Core
import           Snap.Http.Server

data Key a = Key(a, a) deriving (Eq, Generic)
instance Hashable a => Hashable (Key a)

type StringMap = M.Map ByteString ByteString

-- 64MB ought to be big enough for now
max_body_size = 1024 * 1024 * 64

get' :: Snap () -> Snap ()
get' a = method GET a
post' :: Snap () -> Snap ()
post' a = method POST a
del' :: Snap () -> Snap ()
del' a = method DELETE a

main :: IO ()
main = do theMap <- (M.empty :: IO StringMap)
          quickHttpServe $ site theMap

site :: StringMap ->  Snap ()
site m = pathArg $ \k -> (handler k m) <|> invalidRequest

handler :: ByteString -> StringMap -> Snap ()
handler k m = do getKeyHandler k m <|>
                      postKeyHandler k m <|>
                      delKeyHandler k m <|>
                      invalidRequest

getKeyHandler :: ByteString -> StringMap -> Snap ()
getKeyHandler k m = get' $ getKey k m

postKeyHandler :: ByteString -> StringMap -> Snap ()
postKeyHandler k m = post' $ do body <- readRequestBody max_body_size
                                putKey k (toStrict body) m

delKeyHandler :: ByteString -> StringMap -> Snap ()
delKeyHandler k m = del' $ do liftIO $ M.delete k m
                              writeBS $ append "Key Deleted" k

getKey :: ByteString -> StringMap -> Snap ()
getKey k m = do v <- liftIO $ M.lookup k m
                maybe (notFound k) writeBS v

putKey :: ByteString -> ByteString -> StringMap -> Snap ()
putKey k v m = do liftIO $ M.insert k v m
                  created

delKey :: ByteString -> StringMap -> Snap ()
delKey k m = do liftIO $ M.delete k m
                noContent

notFound :: ByteString -> Snap ()
notFound a = do modifyResponse (setResponseCode 404)
                writeBS $ append "Not Found: " a

invalidRequest :: Snap ()
invalidRequest = do modifyResponse (setResponseCode 400)
                    writeBS "Invalid Request"

noContent :: Snap ()
noContent = do modifyResponse (setResponseCode 204)
               writeBS "No Content"

created :: Snap ()
created = do modifyResponse (setResponseCode 201)
             writeBS "Created"

