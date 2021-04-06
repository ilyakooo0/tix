module Data.Map.Shifted.Strict
  ( ShiftedMap (..),
    Data.Map.Shifted.Strict.lookup,
    insert,
    shift,
    liftMap,
    unshift,
  )
where

import Data.Act
import Data.Group
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data ShiftedMap d k v = ShiftedMap d (Map k v)
  deriving stock (Eq, Ord, Show)

instance (Act d v, Group d, Ord k) => Semigroup (ShiftedMap d k v) where
  (ShiftedMap d m) <> (ShiftedMap d' m') =
    ShiftedMap d $ m <> (act (invert d <> d') <$> m')

instance (Monoid d, Semigroup (ShiftedMap d k v)) => Monoid (ShiftedMap d k v) where
  mempty = ShiftedMap mempty M.empty

lookup :: (Ord k, Act d v) => k -> ShiftedMap d k v -> Maybe v
lookup k (ShiftedMap d m) = act d <$> M.lookup k m

insert :: (Ord k, Act d v, Group d) => k -> v -> ShiftedMap d k v -> ShiftedMap d k v
insert k v (ShiftedMap d m) = ShiftedMap d $ M.insert k ((act . invert) d v) m

shift :: (Semigroup d) => d -> ShiftedMap d k v -> ShiftedMap d k v
shift d' (ShiftedMap d m) = ShiftedMap (d <> d') m

liftMap :: Monoid d => Map k v -> ShiftedMap d k v
liftMap = ShiftedMap mempty

unshift :: Act d v => ShiftedMap d k v -> Map k v
unshift (ShiftedMap d m) = act d <$> m
