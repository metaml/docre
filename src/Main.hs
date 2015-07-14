{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStrLn, getLine, words, concat)
import qualified GHC.IO.Exception as Ex
import Control.Exception (try, throwIO)
import Control.Monad (unless, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.ByteString.Lazy (concat, ByteString)
import Data.ByteString.Lazy.Char8 (putStrLn, pack, unpack, words)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)    
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer, Pipe)
import Data.Aeson (decode)
import Data.Docker (Event(..))

main :: IO ()
main = runEffect $ docker >-> convert >-> consul'

-- | @todo: handle error cases
docker :: Producer ByteString IO ()
docker = do
  s <- lift $ socket AF_UNIX Stream defaultProtocol
  lift $ connect s $ SockAddrUnix "/var/run/docker.sock"
  _ <- forever $ do
         lift (event s) >>= yield 
  lift $ sClose s
  docker

convert :: Pipe ByteString (Maybe Event) IO ()
convert = do
  j <- await
  yield $ decode j
  convert

consul :: Consumer (Maybe Event) IO ()
consul = do
  e <- await
  s <- lift $ socket AF_UNIX Stream defaultProtocol
  lift $ connect s $ SockAddrUnix "/var/run/docker.sock"
  case e of
    Just ev -> do
      let eId  = encodeUtf8 $ eventId ev
      lift $ sendAll s $ concat ["GET /containers/", eId, "/json HTTP/1.1\n\n"]
      c <- lift $ recv s 4096
      lift $ print c
    Nothing -> lift $ putStrLn "error"
  consul
  
consul' :: Consumer (Maybe Event) IO ()
consul' = do
  e <- await
  x <- lift $ try $ print e
  case x of
    Left e@(Ex.IOError {Ex.ioe_type = t}) ->
      lift $ unless (Ex.ResourceVanished == t) $ throwIO e -- gracefully terminate on a broken pipe error
    Right () ->
      consul'
         
event :: Socket -> IO ByteString
event s = do
  sendAll s "GET /events HTTP/1.1\n\n"
  j <- recv s 4096
  return j

