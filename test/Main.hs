module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Nix
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Tix.Typechecker

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  let testDirectory :: FilePath
      testDirectory = "test/golden/importless"
  findByExtension [".nix"] testDirectory
    <&> localOption (mkTimeout 1000000) . testGroup "Importless tests"
      . fmap
        ( \file ->
            goldenVsStringDiff
              file
              (\ref new -> ["diff", "-u", ref, new])
              (replaceExtension file ".txt")
              ( do
                  inp <- T.readFile file
                  case parseNixTextLoc inp of
                    Failure err -> error $ show err
                    Success r -> do
                      let (t, _, _, _) = getType r
                      return . BSL.fromStrict . T.encodeUtf8 $ showNType t
              )
        )
