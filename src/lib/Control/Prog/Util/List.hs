module Control.Prog.Util.List (groupBy, forceSpine) where

import           Data.List (partition)

-- | Non-stable version of 'Data.List.groupBy'.
--
--   The returned groups contain all elements from the given list that are
--   equal with respect to the given predicate. Therefore, this implementation
--   does not fulfill the property @concat (groupBy p xs) = xs@ unlike
--   'Data.List.groupBy'.
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []       = []
groupBy eq (x : xs) = (x : ys) : groupBy eq zs
  where (ys, zs) = partition (eq x) xs

-- | Forces the evaluation of all list constructors of the given list.
--
--   The evaluation of the list elements is not forced by this function.
--   The returned @()@ value must be forced (e.g. using @seq@) in order
--   for the list to be forced successfully.
forceSpine :: [a] -> ()
forceSpine = foldr (const id) ()
