module Main where

import System.IO (isEOF)
import Control.Monad (unless)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import Control.Monad (unless)
import Pipes

main :: IO ()
main = putStrLn "Hello, World!"

stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF 
  unless eof $ do
    ln <- lift getLine
    yield ln
    stdinLn   

stdoutLn :: Consumer String IO ()
stdoutLn = do
  ln <- await
  x <- lift $ try $ putStrLn ln
  case x of
    Left e@(G.IOError { G.ioe_type = t}) ->
      -- gracefully terminate on a broken pipe error
      lift $ unless (t == G.ResourceVanished) $ throwIO e
    Right () -> stdoutLn
