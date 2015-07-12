{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn, getLine)
import qualified GHC.IO.Exception as G
import System.IO (isEOF)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception (try, throwIO)
import Control.Monad (unless, forever)
import Data.ByteString.Char8 (putStrLn, pack, ByteString)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer)

-- instance FromJSON Event
-- instance ToJSON Event
    
main :: IO ()
main = runEffect $ docker >-> consul

-- | @todo: broken commit fix later
docker :: Producer ByteString IO ()
docker = do
  s <- lift $ socket AF_UNIX Stream defaultProtocol
  lift $ connect s $ SockAddrUnix "/var/run/docker.sock"
  eof <- lift isEOF 
  unless eof $ do
    forever $ do
      l <- lift $ reader s
      yield l
  lift $ sClose s
  docker

reader :: Socket -> IO ByteString
reader s = do
  sendAll s $ pack "GET /events HTTP/1.1\n\n"
  msg <- recv s 4096
  return msg

consul :: Consumer ByteString IO ()
consul = do
  l <- await
  x <- lift $ try $ putStrLn l
  case x of
    Left e@(G.IOError {G.ioe_type = t}) ->
      -- gracefully terminate on a broken pipe error
      lift $ unless (G.ResourceVanished == t) $ throwIO e
    Right () -> consul
