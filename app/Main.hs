module Main (main) where

import qualified Data.Text.IO as T
import Lib
import Nix.Parser
import Tix.Typechecker

main :: IO ()
main = do
  inp <- T.getContents
  case parseNixTextLoc inp of
    Failure err -> error $ show err
    Success r -> do
      let (t, errs, uErrs) = getType r
      print errs
      print uErrs
      T.putStrLn $ showNType t
