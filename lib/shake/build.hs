import System.Exit (exitSuccess)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension)
import System.FSNotify (watchTree, withManager)
import System.INotify
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Development.Shake.FilePath ((<.>), (</>), takeFileName)
import Development.Shake

main :: IO ()
main = shakeArgs shakeOptions $ do
  "app" ~> do
    cmd "cabal build app"
  "cc" ~> do
    (Exit _) <- cmd "cabal build app" -- ignore errors
    dirs <- liftIO $ lookupEnv "SRC"
    let ds = words $ fromMaybe "src/" dirs
    liftIO $ watch ds
  "run" ~> do
    need ["app"]
    cmd "dist/build/app/app"
  "update" ~> do
    cmd "cabal install --only-dependencies"
  "shake" ~> do
    hs <- getDirectoryFiles "lib/shake" ["//*.hs"]
    need $ map (\(h,_) -> "bin" </> dropExtension h) $ hns hs
  "bin/*" %> \bin -> do -- /* is not cool
    let h = "lib/shake" </> takeFileName bin
    cmd "cabal exec -- ghc -o" [bin, h <.> "hs"]
  "clean" ~> do
    removeFilesAfter "lib/shake" ["//*.o","//*.hi"]
    removeFilesAfter "dist" ["//*"]
  "clobber" ~> do
    need ["clean"]
    removeFilesAfter "dist" ["//*"]
    removeFilesAfter "bin" ["//*"]

type ShakeSrc = String
type Name = String

hns :: [ShakeSrc] -> [(ShakeSrc, Name)]
hns ss = let ns = map (\f -> dropExtension (last $ splitOn "/" f)) ss
         in zip ss (map (\n -> map toLower n) ns)

-- @todo: implement the following using FSNotify
watch :: [FilePath] -> IO ()
watch fs = withINotify $ \inotify -> do
  let fileEvents = [CloseWrite, Delete, Move]
  wds <- mapM (\f -> addWatch inotify fileEvents f (handleEvent f)) fs
  putStrLn "compling continuously"
  _ <- ($) forever $ threadDelay (3600*1000000)
  mapM_ (\wd -> removeWatch wd) wds
  where handleEvent :: FilePath -> Event -> IO ()
        handleEvent _ _ = cmd "cabal build app" 
