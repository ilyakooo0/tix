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

-- pattern EmptyRange :: Range a
-- pattern EmptyRange = Range' []

-- pattern Range :: Ord a => a -> a -> Range a -> Range a
-- pattern Range l r rest <-
--   ( ( \x -> case x of
--         Range' ((a, b) : rest) -> Just (a, b, Range' rest)
--         _ -> Nothing
--     ) ->
--       Just (l, r, rest)
--     )

-- addToRange :: Ord a => a -> a -> Range a -> Range a
-- addToRange _ _ InfiniteRange = InfiniteRange
-- addToRange l r (Range' rs) =
