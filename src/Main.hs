{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStrLn, getLine, words, concat)
import qualified GHC.IO.Exception as Ex
import Control.Exception (try, throwIO)
import Control.Monad (unless, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy (concat, split, ByteString)
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer, Pipe)
import Data.Aeson (decode)
import Data.Docker (Event(..))

main :: IO ()
main = runEffect $ docker >-> json2event >-> event2Id >-> id2container >-> consul

-- | @todo: handle error cases
docker :: Producer ByteString IO ()
docker = do
  s <- lift $ socket AF_UNIX Stream defaultProtocol
  lift $ connect s $ SockAddrUnix "/var/run/docker.sock"
  _ <- forever $ lift (event s) >>= yield 
  lift $ sClose s
  docker

json2event :: Pipe ByteString Event IO ()
json2event = do
  r <- await
  let j = (split (c2w '\n') r) !! 1
  lift $ putStrLn j
  case (decode j) of
    Just e -> yield e
    Nothing -> lift $ putStrLn "### error json2event ###" >> print j
  json2event

event2Id :: Pipe Event ByteString IO ()
event2Id = do
  e <- await
  yield $ encodeUtf8 $ eventId e
  event2Id

id2container :: Pipe ByteString ByteString IO ()
id2container = do
  eId <- await
  s <- lift $ socket AF_UNIX Stream defaultProtocol
  lift $ connect s $ SockAddrUnix "/var/run/docker.sock"
  lift $ sendAll s $ concat ["GET /containers/", eId, "/json HTTP/1.1\r\n\r\n"]
  r <- lift $ recv s 4096
  let j = (split (c2w '\n') r) !! 6
  lift $ putStrLn "id2container:" >> print j
  id2container

consul :: Consumer ByteString IO ()
consul = do
  json <- await
  lift $ putStrLn json
  consul
  
event :: Socket -> IO ByteString
event s = do
  sendAll s "GET /events HTTP/1.1\r\n\r\n"
  j <- recv s 4096
  return j

-- | parse a raw HTTP ByteString
response :: ByteString -> ByteString
response r = last $ split (c2w '\n') r

-- | below: only for reference
consul' :: Consumer (Maybe Event) IO ()
consul' = do
  ev <- await
  x <- lift $ try $ print ev
  case x of
    Left e@(Ex.IOError {Ex.ioe_type = t}) ->
      lift $ unless (Ex.ResourceVanished == t) $ throwIO e -- gracefully terminate on a broken pipe error
    Right () ->
      consul'


