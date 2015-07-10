module Main where

import System.IO (openFile, isEOF, hGetContents, hClose, Handle, IOMode(ReadMode))
import Control.Monad (unless)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import Control.Monad (unless)
import Pipes

main :: IO ()
main = runEffect $ producer >-> consumer

-- | @todo: broken commit fix later
producer :: Producer String IO ()
producer = do
  eof <- lift isEOF 
  h <- openFile "/var/run/docker.sock" ReadMode
  withFile h reader (\s -> do
    c <- hGetContents h
    yield c
    return ()
    producer)

reader :: Handle -> IO (a)
reader h = do
  return $ hGetContents h

consumer :: Consumer String IO ()
consumer = do
  l <- await
  x <- lift $ try $ putStrLn ("consumer: " ++ l)
  case x of
    Left e@(G.IOError { G.ioe_type = t}) ->
      -- gracefully terminate on a broken pipe error
      lift $ unless (t == G.ResourceVanished) $ throwIO e
    Right () -> consumer

withFile :: Handle -> (Handle -> IO a) -> IO a
withFile h f = do  
  --- h <- openFile path mode   
  r <- f h
  --- hClose h
  return r

producer' :: Producer String IO ()
producer' = do
  eof <- lift isEOF 
  unless eof $ do
    l <- lift getLine
    yield l
    producer

consumer' :: Consumer String IO ()
consumer' = do
  l <- await
  x <- lift $ try $ putStrLn ("consumer: " ++ l)
  case x of
    Left e@(G.IOError { G.ioe_type = t}) ->
      -- gracefully terminate on a broken pipe error
      lift $ unless (t == G.ResourceVanished) $ throwIO e
    Right () -> consumer

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
  h <- openFile path mode   
  r <- f h
  hClose h
  return r
