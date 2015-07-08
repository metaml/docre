module Main (main) where

import Development.Shake
import System.Environment (lookupEnv)
import System.INotify
import System.Process (system)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)

main :: IO ()
main = shakeArgs shakeOptions $ do
         phony "cc" $ do
           dirs <- liftIO $ lookupEnv "SRC_DIRS"
           let ds = words $ fromMaybe "" dirs
           liftIO $ watch ds

watch :: [FilePath] -> IO ()
watch fs = withINotify $ \inotify -> do
  let fileEvents = [Modify, CloseWrite
                   , Move, MoveIn, MoveOut, MoveSelf
                   , Create, Delete, DeleteSelf
                   ]
  wds <- mapM (\f -> addWatch inotify fileEvents f (handleEvent f)) fs
  putStrLn "hit \"return\" to terminate:" >> getLine
  mapM_ (\wd -> removeWatch wd) wds
  where
    handleEvent :: FilePath -> Event -> IO ()
    handleEvent f e = do
      _ <- system "cabal build app"
      return ()

