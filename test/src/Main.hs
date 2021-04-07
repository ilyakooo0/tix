module Main (main) where

import Data.Functor
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as TL
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
      testDirectory = "test/golden"
  findByExtension [".nix"] testDirectory
    <&> localOption (mkTimeout 10000000) . testGroup "Importless tests"
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
                      case getType r of
                        (t, [], [], [], _) ->
                          return $ (TL.encodeUtf8 . renderPretty) t
                        (_, errors, unifyingErrors, predicateErrors, _) -> error . show $ (errors, unifyingErrors, predicateErrors)
              )
        )
