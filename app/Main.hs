module Main (main) where

import qualified Data.Text.IO as Text
import Lib
import Nix.Parser
import Tix.Typechecker

main :: IO ()
main = do
  inp <- Text.getContents
  case parseNixTextLoc inp of
    Failure err -> error $ show err
    Success r -> case getType r of
      (Right x, _) -> print x
      (Left e, _) -> print e
