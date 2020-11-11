module Data.Range
  ( Range (Range, InfiniteRange, EmptyRange),
    member,
    notMember,
  )
where

-- | Left included, right excluded
data Range a
  = Range' !a !a
  | InfiniteRange
  | EmptyRange

instance Show a => Show (Range a) where
  show InfiniteRange = "∞"
  show EmptyRange = "∅"
  show (Range' l r) = show l <> "..<" <> show r

member :: Ord a => a -> Range a -> Bool
member _ InfiniteRange = True
member _ EmptyRange = False
member a (Range' l r) = a >= l && a < r
{-# INLINE member #-}

notMember :: Ord a => a -> Range a -> Bool
notMember a r = not $ member a r
{-# INLINE notMember #-}

pattern Range :: Ord a => a -> a -> Range a
pattern Range x y <- Range' x y where Range x y = Range' (min x y) (max x y)
