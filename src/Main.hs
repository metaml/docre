{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn, getLine)
import qualified GHC.IO.Exception as G
import System.IO (isEOF)
import Control.Exception (try, throwIO)
import Control.Monad (unless)
import Data.ByteString.Char8
import Pipes

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
reader = getLine

consul :: Consumer ByteString IO ()
consul = do
  l <- await
  x <- lift $ try $ putStrLn l
  case x of
    Left e@(G.IOError {G.ioe_type = t}) ->
      -- gracefully terminate on a broken pipe error
      lift $ unless (G.ResourceVanished == t) $ throwIO e
    Right () -> consul
