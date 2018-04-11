module InteractionNetwork
  ( adjacencyMatrix
  ) where

import Numeric.LinearAlgebra.Sparse

type Snapshot = SpMatrix Double
type Network = [Snapshot]

adjacencyMatrix :: [(Int,Int)] -> Snapshot
adjacencyMatrix = fromListSM . map (\(x, y) -> (x,y,1))
