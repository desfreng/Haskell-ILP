{-# LANGUAGE NamedFieldPuns #-}

module Solver.Indexed where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Indexed a = Ix
  { intToA :: !(IntMap a),
    aToInt :: !(Map a Int)
  }

getIndex :: (Ord a) => Indexed a -> a -> Int
getIndex Ix {aToInt} x = aToInt M.! x

setIndex :: (Ord a) => Indexed a -> a -> Int -> Indexed a
setIndex Ix {intToA, aToInt} = undefined

fromIndex :: Indexed a -> Int -> a
fromIndex Ix {intToA} x = intToA IM.! x
