import Development.Shake
import System.FilePath (dropExtension)
import Data.Char (toLower)
import Data.List.Split (splitOn)
--import Development.Shake.Command
import Development.Shake.FilePath
--import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions $ do
         phony "build" $ do
           cmd "cabal build app"
         phony "bin" $ do
           hs <- getDirectoryFiles "lib/shake" ["//*.hs"]
           let hns = fns hs
               cs = map (\p -> "cabal exec -- ghc -o bin/" ++ (snd p) ++ " lib/shake/" ++ (fst p)) hns
           need $ map (\(h,n) -> "bin" </> dropExtension h) hns
         phony "clean" $ do
           removeFilesAfter "dist" ["//*"]
         "bin/*" %> \bin -> do
           let h = "lib/shake" </> takeFileName bin
           cmd "cabal exec -- ghc -o" [bin, h <.> "hs"]

fns :: [String] -> [(String, String)]
fns fs = let fs' = map (\f -> dropExtension (last $ splitOn "/" f)) fs
         in zip fs (map (\word -> map toLower word) fs')

