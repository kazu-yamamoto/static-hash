{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH.Prime
import Test.Framework.Providers.QuickCheck2
import qualified Data.Map as M
import qualified Data.StaticHash as S

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

prop_model :: [(String,Int)] -> Bool
prop_model kvs = and $ for keys $ \key ->
    S.lookup key ht == M.lookup key mp
  where
    len = length kvs
    kvs' = take (len `div` 2) kvs
    ht = S.fromList kvs'
    mp = M.fromList kvs'
    keys = map fst kvs
    for = flip map
