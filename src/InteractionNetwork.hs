module InteractionNetwork
  ( adjacencyMatrix
  , interactionNetwork
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
