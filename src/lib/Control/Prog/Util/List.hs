module Control.Prog.Util.List (groupBy) where

import           Data.List (partition)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []       = []
groupBy eq (x : xs) = (x : ys) : groupBy eq zs
  where (ys, zs) = partition (eq x) xs
