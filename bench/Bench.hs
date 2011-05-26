module Main where

import Control.DeepSeq
import Criterion.Main
import qualified Data.HashMap as M (lookup, fromList)
import Data.List hiding (lookup)
import Data.StaticHash
import Prelude hiding (lookup)

main :: IO ()
main = let !target = permutations "abcdefgh"
           !pair = target `deepseq` map (\x -> (x,x)) target
           !staticHash = pair `deepseq` fromList pair
           !hashMap = pair `deepseq` M.fromList pair
       in defaultMain [
           bgroup "lookup" [
                bench "M.lookup" (nf (map (flip M.lookup hashMap)) target)
              , bench "S.lookup" (nf (map (flip lookup staticHash)) target)
              ]
           ]
