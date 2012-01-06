{-|
  Pure immutable hash whose lookup is O(1) on the average,
  but O(N) in the worst case.
-}

module Data.StaticHash (
    StaticHash
  , fromList, fromList', lookup
  ) where

import Data.Array
import Data.Function
import Data.Hashable
import Data.List (groupBy,sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Numbers.Primes
import Data.Ord
import Prelude hiding (lookup)

----------------------------------------------------------------

{-|
  Data type for immutable hashes.
-}

newtype StaticHash k v = StaticHash (Array Int (Some k v)) deriving Show

data Some k v = None | One k v | More (Map k v) deriving Show

type Hash = Int

----------------------------------------------------------------

{-|
  Creating 'StaticHash' from a list. A prime around the length of
  the list x 2 is chosen for the size of array. This may prevent
  collisions.
-}

fromList :: (Eq k, Ord k, Hashable k) => [(k,v)] -> StaticHash k v
fromList xs = fromList' (length xs) xs

{-|
  Creating 'StaticHash' from a list and its size.
-}

fromList' :: (Eq k, Ord k, Hashable k) => Int -> [(k,v)] -> StaticHash k v
fromList' len xs = StaticHash $ array (0,p-1) $ toIxKV xs
  where
    threshold = len * 2 -- hoping collision free
    p = findPrime threshold
    hashfunc = (`hashBy` p)
    toIxKV = fil 0 p . unify . hashGroup hashfunc

findPrime :: Int -> Int
findPrime threshold = head $ filter (>= threshold) primes

hashGroup :: (Eq k, Ord k, Hashable k) => (k -> Hash) -> [(k,v)] -> [[(Hash,(k,v))]]
hashGroup hashfunc xs = gs
  where
    hs = map (\kv@(k,_) -> (hashfunc k, kv)) xs
    gs = groupBy ((==) `on` fst) $ sortBy (comparing fst) hs

unify :: Ord k => [[(Hash, (k, v))]] -> [(Hash, Some k v)]
unify = map hashKeyVal

hashKeyVal :: Ord k => [(Hash,(k,v))] -> (Hash,Some k v)
hashKeyVal xs = (h, toSome kvs)
  where
    (h:_,kvs) = unzip xs

toSome :: Ord k => [(k,v)] -> Some k v
toSome []      = None
toSome [(k,v)] = One k v
toSome kvs     = More (M.fromList kvs)

fil :: Int -> Int -> [(Hash, Some k v)] -> [(Hash, Some k v)]
fil i lim []
  | i < lim   = (i,None) : fil (i+1) lim []
  | otherwise = []
fil i lim kvs@((k,v):rest)
  | i < k     = (i,None) : fil (i+1) lim kvs
  | otherwise = (k,v)    : fil (k+1) lim rest

----------------------------------------------------------------

{-|
  Looking up 'StaticHash'.
-}
lookup :: (Eq k, Ord k, Hashable k) => k -> StaticHash k v -> Maybe v
lookup key (StaticHash hs) = case hs ! i of
    None    -> Nothing
    One k v
      | key == k  -> Just v
      | otherwise -> Nothing
    More m  -> M.lookup key m
  where
    (_,p') = bounds hs
    p = p' + 1
    i = key `hashBy` p

----------------------------------------------------------------

hashBy :: Hashable k => k -> Int -> Int
hashBy k p = hash k `mod` p
