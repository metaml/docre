import System.FilePath (dropExtension)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Development.Shake.FilePath ((<.>), (</>), takeFileName)
import Development.Shake

main :: IO ()
main = shakeArgs shakeOptions $ do
         "build" ~> do
           cmd "cabal build app"
         "shake" ~> do
           hs <- getDirectoryFiles "lib/shake" ["//*.hs"]
           need $ map (\(h,_) -> "bin" </> dropExtension h) $ hns hs
         "bin/*" %> \bin -> do -- /* is not cool
           let h = "lib/shake" </> takeFileName bin
           cmd "cabal exec -- ghc -o" [bin, h <.> "hs"]
         "clean" ~> do
           removeFilesAfter "dist" ["//*"]
           removeFilesAfter "bin" ["//*"]

type HaskellSrc = String
type Name = String

hns :: [HaskellSrc] -> [(HaskellSrc, Name)]
hns hs = let ns = map (\f -> dropExtension (last $ splitOn "/" f)) hs
         in zip hs (map (\n -> map toLower n) ns)

