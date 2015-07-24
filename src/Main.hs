{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStrLn, getLine, words, lines, concat, length, drop, dropWhile, null, lookup)
import qualified GHC.IO.Exception as Ex
import qualified Data.HashMap.Strict as Map
import Control.Exception (try, throwIO)
import Control.Monad (unless, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson (decodeStrict)
import Data.Aeson.Types
import Data.ByteString (concat, length, drop, null, ByteString)
import Data.ByteString.Char8 (putStrLn, breakSubstring)
import Data.Text (dropWhile, Text)
import Data.Text.Encoding (encodeUtf8)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer, Pipe)
import Data.Docker (Event(..))

main :: IO ()
main = runEffect $ docker >-> json2event >-> event2id >-> id2container >-> container2consul >-> consul

type Status = ByteString
type Cid = ByteString
type Name = ByteString
type Ip = ByteString        
type Services = [ByteString]
    
-- | @todo: handle error cases
docker :: Producer ByteString IO ()
docker = forever $ do
           s <- lift $ unixSocket       
           _ <- ($) forever $ lift (event s) >>= yield 
           lift $ sClose s

json2event :: Pipe ByteString Event IO ()
json2event = forever $ do
               r <- await
               lift $ putStrLn "- json2event:" >> print r
               let j = last (tokenize "\r\n" r)
               case (decodeStrict j) of
                 Nothing -> lift $ putStrLn "error in json2event:" >> print j
                 Just e -> yield e

event2id :: Pipe Event (Cid, Status) IO ()
event2id = forever $ do
             e <- await
             yield $ (encodeUtf8 $ eventId e, encodeUtf8 $ eventStatus e)

id2container :: Pipe (Cid, Status) (ByteString, Status) IO ()
id2container = forever $ do
                 s <- lift $ unixSocket
                 _ <- ($) forever $ do
                           (eId, status) <- await
                           lift $ sendAll s $ concat ["GET /containers/", eId, "/json HTTP/1.1", "\r\n\r\n"]
                           r <- lift $ recv s 4096 -- r: http response
                           lift $ putStrLn "- id2container:" >> print r
                           yield $ (r, status)
                 lift $ sClose s

container2consul :: Pipe (Cid, Status) (Object, Status) IO ()
container2consul = forever $ do
                     (r, status) <- await  -- r: http response
                     let o :: Maybe Object = decodeStrict (last $ tokenize "\r\n" r)
                     case o of
                       Nothing -> lift $ putStrLn "error in container2consul"
                       Just e -> yield (e, status)
                                 
consul :: Consumer (Object, Status) IO ()
consul = forever $ do
           (h, status) <- await
           let (Just (String cid)) = Map.lookup "Id" h
               (Just (String name)) = Map.lookup "Name" h
               Just net = Map.lookup "NetworkSettings" h
               (Just (String ip)) = ipAddress net
               name' = dropWhile (\c -> c == '/') name
--           case status of
--             "start" -> lift $ undefined cid name' ip ["8080"]
--             "stop" -> lift $ undefined cid name' ["8080"]
--             _ -> lift $ return ()
           lift $ print status >> print cid >> print name >> print ip

type Status' = Text
type Cid' = Text
type Name' = Text
type Ip' = Text        
type Services' = [Text]
           
event :: Socket -> IO ByteString
event s = do
  sendAll s "GET /events HTTP/1.1\r\nContent-Type: application/json\r\n\r\n"
  j <- recv s 4096
  return j

unixSocket :: IO Socket
unixSocket = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s $ SockAddrUnix "/var/run/docker.sock"
  return s

ipAddress :: Value -> Maybe Value
ipAddress (Object obj) = Map.lookup "IPAddress" obj
ipAddress _ = Nothing

-- @todo: refactor
tokenize :: ByteString -> ByteString -> [ByteString]
tokenize d bs = filter (\e -> e /= "") $ h : if null t then [] else tokenize d (drop (length h) t)
                                             where (h, t) = breakSubstring d bs

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
