module Data.Map.Extra
  ( restrictKeysToRange,
  )
where

import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Range (Range (..))
import qualified Data.Range as R

restrictKeysToRange :: Ord k => Range k -> Map k v -> Map k v
restrictKeysToRange range =
  M.takeWhileAntitone (`R.member` range)
    . M.dropWhileAntitone (`R.notMember` range)
