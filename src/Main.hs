{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStrLn, getLine, words)
import qualified GHC.IO.Exception as G
import Control.Exception (try, throwIO)
import Control.Monad (unless, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy.Char8 (putStrLn, pack, unpack, words, ByteString)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer, Pipe)
import Data.Aeson (encode, decode)
import Data.Docker

main :: IO ()
main = runEffect $ docker >-> convert >-> consul

-- | @todo: handle error cases
docker :: Producer ByteString IO ()
docker = do
  s <- lift $ socket AF_UNIX Stream defaultProtocol
  lift $ connect s $ SockAddrUnix "/var/run/docker.sock"
  _ <- forever $ do
         lift (json s) >>= yield 
  lift $ sClose s
  docker

convert :: Pipe ByteString (Maybe Event) IO ()
convert = do
  j <- await
  _ <- lift $ print j
  yield $ decode j
  convert
  
consul :: Consumer (Maybe Event) IO ()
consul = do
  ev <- await
  x <- lift $ try $ print ev
  case x of
    Left e@(G.IOError {G.ioe_type = t}) ->
      lift $ unless (G.ResourceVanished == t) $ throwIO e -- gracefully terminate on a broken pipe error
    Right () ->
      consul
         
json :: Socket -> IO ByteString
json s = do
  sendAll s "GET /events HTTP/1.1\n\n"
  j <- recv s 4096
  return j

