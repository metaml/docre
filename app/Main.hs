module Main where

import System.Directory (doesFileExist)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.Trans.Either (left, runEitherT)
import Pipes (lift)
import Pipes.Docker (dockerListener)

main :: IO ()
main = do
  e <- runEitherT $ forever $ do
         sock <- lift $ doesFileExist "/var/run/docker.sock"
         _ <- lift $ print sock >> threadDelay 1000000
         when sock $ left ()
  case e of
    Left  _ -> dockerListener
    Right _ -> main
