{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn, getLine)
import qualified GHC.IO.Exception as G
import System.IO (isEOF)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception (try, throwIO)
import Control.Monad (unless)
import Data.ByteString.Char8 (putStrLn, pack, ByteString)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer)

main :: IO ()
main = runEffect $ docker >-> consul

-- | @todo: broken commit fix later
docker :: Producer ByteString IO ()
docker = do
  eof <- lift isEOF 
  unless eof $ do
    l <- lift reader
    yield l
    docker

reader :: IO ByteString
reader = do
  putStrLn "start reading"
  addrs <- getAddrInfo Nothing (Just "google.com") (Just "80")
  let server = head addrs
  s <- socket (addrFamily server) Stream defaultProtocol
  connect s (addrAddress server)
  sendAll s $ pack "GET /\n\n"
  msg <- recv s 1024
  sClose s
  putStrLn "done reading"
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
