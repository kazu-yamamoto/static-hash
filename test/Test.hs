{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH
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
    ht = S.fromList kvs
    mp = M.fromList kvs
    keys = map fst kvs
    for = flip map
