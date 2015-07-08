import Development.Shake
import System.FilePath (dropExtension)
import Text.Printf (printf)
import Data.Char (toLower)
import Data.Text (pack)
import Data.List.Split (splitOn)
--import Development.Shake.Command
--import Development.Shake.FilePath
--import Development.Shake.Util

fns :: [String] -> [(String, String)]
fns fs = let fs' = map (\f -> dropExtension (last $ splitOn "/" f)) fs
         in zip fs (map (\word -> map toLower word) fs')

main :: IO ()
main = shakeArgs shakeOptions $ do
         phony "build" $ do
           cmd "cabal build main"
         phony "shake" $ do
           fs' <- getDirectoryFiles "src/Util/Shake" ["//*.hs"]
           liftIO $ print fs'
           let fns' = fns fs'
               --cs = map (\fn -> printf "cabal exec -- ghc --make -o ./bin/%s src/Util/Shake/%s" (snd fn) (fst fn)) fns'
               cs = map (\p -> printf "cabal exec runghc ghc --make -o %s src/Util/Shake/%s" (snd p) (fst p)) fns'
           cmd "echo" (snd $ head fns')
         phony "clean" $ do
           removeFilesAfter "dist" ["//*"]
