{-|
  Pure immutable hash whose lookup is O(1).
-}

module Data.StaticHash (
    StaticHash
  , fromList, lookup
  ) where

import Data.Array
import Data.Hashable
import Data.List (groupBy,sortBy)
import Data.Numbers.Primes
import qualified Prelude as P (lookup)
import Prelude hiding (lookup)

----------------------------------------------------------------

{-|
  Data type for immutable hashes.
-}
newtype StaticHash k v = StaticHash (Array Int (Maybe [(k,v)]))

----------------------------------------------------------------

{-| 
  Creating 'StaticHash' from a list. A prime around the length of
  the list x 2 is chosen for the size of array. This may prevent
  collisions.
-}

fromList :: (Eq k,Hashable k) => [(k,v)] -> StaticHash k v
fromList xs = StaticHash $ array (0,p-1) ls
  where
    len = length xs
    threshold = len * 2 -- hoping collision free
    p = head $ filter (>= threshold) primes
    hs = map (\xy -> (fst xy `hashBy` p, xy)) xs
    x <=> y = fst x`compare` fst y
    x === y = fst x == fst y
    gs = groupBy (===) $ sortBy (<=>) hs
    unify ys = (fst (head ys), map snd ys)
    ls = fil 0 p $ map unify gs

fil :: Int -> Int -> [(Int, v)] -> [(Int, Maybe v)]
fil i lim []
  | i < lim   = (i,Nothing) : fil (i+1) lim []
  | otherwise = []
fil i lim kvs@((k,v):rest)
  | i < k     = (i,Nothing) : fil (i+1) lim kvs
  | otherwise = (k,Just v)  : fil (k+1) lim rest

----------------------------------------------------------------

{-|
  Looking up 'StaticHash'.
-}
lookup :: (Eq k,Hashable k) => k -> StaticHash k v -> Maybe v
lookup k (StaticHash hs) = hs ! i >>= P.lookup k
  where
    (_,p') = bounds hs
    p = p' + 1
    i = k `hashBy` p

----------------------------------------------------------------

hashBy :: Hashable k => k -> Int -> Int
hashBy k p = hash k `mod` p
