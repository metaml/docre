{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStrLn, getLine, words, lines, concat, length, drop, null)
import qualified GHC.IO.Exception as Ex
import Control.Exception (try, throwIO)
import Control.Monad (unless, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson as Aeson
import Data.ByteString (concat, length, drop, null, ByteString)
import Data.ByteString.Char8 (putStrLn, breakSubstring)
import Data.Text.Encoding (encodeUtf8)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer, Pipe)
import Data.Docker (Event(..))

main :: IO ()
main = runEffect $ docker >-> json2event >-> event2Id >-> id2container >-> consul

-- | @todo: handle error cases
docker :: Producer ByteString IO ()
docker = forever $ do
           s <- lift $ unixSocket       
           _ <- ($) forever $ lift (event s) >>= yield 
           lift $ sClose s

json2event :: Pipe ByteString Event IO ()
json2event = forever $ do
               r <- await
               let j = last (tokenize "\r\n" r)
               lift $ putStrLn "json2event:" >> print j
               case (Aeson.decodeStrict j) of
                 Nothing -> lift $ putStrLn "error in json2event:" >> print j
                 Just e -> yield e

event2Id :: Pipe Event ByteString IO ()
event2Id = forever $ do
             e <- await
             yield $ encodeUtf8 $ eventId e 

id2container :: Pipe ByteString ByteString IO ()
id2container = forever $ do
                 s <- lift $ unixSocket
                 _ <- ($) forever $ do
                           eId <- await
                           lift $ sendAll s $ concat ["GET /containers/", eId, "/json HTTP/1.1", "\r\n\r\n"]
                           r <- lift $ recv s 4096
                           yield $ last (tokenize "\r\n" r)
                 lift $ sClose s

consul :: Consumer ByteString IO ()
consul = forever $ do
           j <- await
           lift $ putStrLn j
  
event :: Socket -> IO ByteString
event s = do
  sendAll s "GET /events HTTP/1.1\r\n\r\n"
  j <- recv s 4096
  return j

unixSocket :: IO Socket
unixSocket = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s $ SockAddrUnix "/var/run/docker.sock"
  return s     
         
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
