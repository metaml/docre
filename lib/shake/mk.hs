module Main (main) where

import System.Environment (lookupEnv)
import System.INotify
import Prelude hiding (catch)
import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO, AsyncException(UserInterrupt))
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Development.Shake

main :: IO ()
main = shakeArgs shakeOptions $ do
         "cc" ~> do
           dirs <- liftIO $ lookupEnv "SRC"
           let ds = words $ fromMaybe "src" dirs
           liftIO $ watch ds

watch :: [FilePath] -> IO ()
watch fs = withINotify $ \inotify -> do
  let fileEvents = [Modify, CloseWrite,
                    Move, MoveIn, MoveOut, MoveSelf,
                    Create, Delete, DeleteSelf
                   ]
  wds <- mapM (\f -> addWatch inotify fileEvents f (handleEvent f)) fs
  putStrLn "compling continuously"
  forever $ threadDelay (3600*10^6)
  mapM_ (\wd -> removeWatch wd) wds
  where
    handleEvent :: FilePath -> Event -> IO ()
    handleEvent f e = do
      () <- cmd "cabal build app"
      return ()

