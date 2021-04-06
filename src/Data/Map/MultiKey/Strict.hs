module Data.Map.MultiKey.Strict
  ( Keyable (..),
    Map (..),
    singleton,
    delete,
    lookup,
    lookupEmpty,
    insert,
    partition,
    fromList,
    toList,
    elems,
    keys,
    map,
    toMap,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Prelude hiding (lookup, map)

class (Ord (Key t), Ord t) => Keyable t where
  type Key t :: Type
  getKeys :: t -> [Key t]

data Map t = Map (M.Map (Key t) (S.Set t)) (S.Set t)

instance (Ord (Key t), Ord t) => Semigroup (Map t) where
  (Map x x') <> (Map y y') = Map (M.unionWith S.union x y) (x' <> y')

instance (Ord (Key t), Ord t) => Monoid (Map t) where
  mempty = Map M.empty S.empty

map :: (Keyable h, Ord t) => (t -> h) -> Map t -> Map h
map f = fromList . fmap f . toList

deriving stock instance (Show (Key t), Show t) => Show (Map t)

singleton :: Keyable t => t -> Map t
singleton t = case getKeys t of
  [] -> Map M.empty (S.singleton t)
  ks@(_ : _) -> Map (M.fromList $ fmap (,s) ks) S.empty
  where
    s = S.singleton t

insert :: Keyable t => t -> Map t -> Map t
insert t m = m <> singleton t

lookup :: Keyable t => Key t -> Map t -> S.Set t
lookup k (Map m _) = fromMaybe S.empty $ M.lookup k m

lookupEmpty :: Map t -> S.Set t
lookupEmpty (Map _ s) = s

fromList :: Keyable t => [t] -> Map t
fromList = foldMap singleton

elems :: Ord t => Map t -> S.Set t
elems (Map m s) = (fold . M.elems $ m) <> s

toList :: Ord t => Map t -> [t]
toList = S.toList . elems

keys :: Map t -> S.Set (Key t)
keys (Map m _) = M.keysSet m

toMap :: Ord (Key t) => Map t -> M.Map (Maybe (Key t)) (S.Set t)
toMap (Map m s) = M.insert Nothing s $ M.mapKeysMonotonic Just m

delete :: Keyable t => t -> Map t -> Map t
delete t (Map m empties) = Map (f m) (S.delete t empties)
  where
    s = S.singleton t
    f =
      appEndo $
        foldMap
          ( Endo
              . M.update
                (\s' -> if s == s then Nothing else Just (S.delete t s'))
          )
          (getKeys t)

partition :: Keyable t => (Key t -> Bool) -> Map t -> (Map t, Map t)
partition predicate (Map m empties) = (Map keep' empties, Map toss S.empty <> Map toss' S.empty)
  where
    (keep, toss) = M.partitionWithKey (\k _ -> predicate k) m
    (keep', toss') =
      join bimap M.fromAscList
        . p (join bimap nothingNull . S.partition (all predicate . getKeys))
        . M.toAscList
        $ keep
    p :: (v -> (Maybe v, Maybe v)) -> [(k, v)] -> ([(k, v)], [(k, v)])
    p _ [] = ([], [])
    p f ((k, x) : xs) = join bimap (maybe id ((:) . (k,))) (f x) `apBoth` p f xs
    apBoth :: (a -> b, x -> y) -> (a, x) -> (b, y)
    apBoth (f, g) (a, x) = (f a, g x)
    nothingNull :: S.Set x -> Maybe (S.Set x)
    nothingNull s = if S.null s then Nothing else Just s
