module InteractionNetwork
  ( adjacencyMatrix
  , interactionNetwork
  ) where

import Data.Sparse.SpMatrix
import Numeric.LinearAlgebra.Sparse

type Snapshot = SpMatrix Double
type Network = [Snapshot]

adjacencyMatrix :: Int -> [(Int,Int)] -> Snapshot
adjacencyMatrix numNodes interactions = fromListSM (numNodes,numNodes) $ map (\(x, y) -> (x,y,1)) interactions

interactionNetwork :: Int -> [[(Int, Int)]] -> Network
interactionNetwork size = map (adjacencyMatrix size)
