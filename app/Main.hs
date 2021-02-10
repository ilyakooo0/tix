module Main (main) where

import Data.Aeson ((.=))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.IO as T
import qualified Data.Yaml as YAML
import Nix.Parser
import Tix.Typechecker

main :: IO ()
main = do
  inp <- T.getContents
  case parseNixTextLoc inp of
    Failure err -> error $ show err
    Success r -> do
      let (t, errs, uErrs, o) = getType r
      BSC.putStrLn . YAML.encode $
        o <> "errors" .= fmap show errs
          <> "unifying_errors" .= fmap show uErrs
          <> "resulting_type" .= showNType t
