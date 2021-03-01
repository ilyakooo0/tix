module Data.Map.Extra
  ( restrictKeysToRange,
  )
where

import qualified Data.Map as M
import Data.Map.Strict (Map)
import qualified Data.RangeSet.Map as RS

restrictKeysToRange :: Ord k => RS.RSet k -> Map k v -> Map k v
restrictKeysToRange range =
  M.takeWhileAntitone (`RS.member` range)
    . M.dropWhileAntitone (`RS.notMember` range)
