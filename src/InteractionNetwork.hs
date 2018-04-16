{-# LANGUAGE BangPatterns #-}
module InteractionNetwork
  ( adjacencyMatrix
  , interactionNetwork
  , tcc
  , InteractionNetwork(..)
  ) where

import Data.Sparse.SpMatrix
import Numeric.LinearAlgebra.Sparse

type Snapshot = SpMatrix Double
type InteractionNetwork = [Snapshot]

adjacencyMatrix :: Int -> [(Int,Int)] -> Snapshot
adjacencyMatrix numNodes interactions = fromListSM (numNodes,numNodes) $ map (\(x, y) -> (x,y,1)) interactions

interactionNetwork :: Int -> [[(Int, Int)]] -> InteractionNetwork
interactionNetwork size = map (adjacencyMatrix size)

-- | Compute the temporal correlation coefficient
tcc :: InteractionNetwork -> Double
tcc net = (sum cis) / fromIntegral n
  where (n,_) = dim $ head net
        timesteps = fromIntegral $ length net
        cis = map (/ timesteps) $ foldr (\ !acc cits -> zipWith (+) cits acc) (repeat 0.0) [ cit t t' | (t,t') <- zip net (tail net) ]
        cit :: Snapshot -> Snapshot -> [Double]
        cit t t' = let numerators  = map (\(_,_,x) -> x) . diagonals . diagPartitions $ t ##^ t'
                       x           = map (sum . map snd) $ (map toListSV) $ toRowsL t
                       x'          = map (sum . map snd) $ (map toListSV) $ toRowsL t'
                   in zipWith3 (\numerator s s' -> numerator / (sqrt $ s * s')) numerators x x'
          where diagonals (_, !d, _) = toListSM d

-- TODO: Compute all pairs shortest paths using generalized BFS
