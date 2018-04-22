{-# LANGUAGE BangPatterns #-}
module InteractionNetwork
  ( adjacencyMatrix
  , interactionNetwork
  , tcc
  , ctpl
  , InteractionNetwork(..)
  ) where

import Data.Sparse.Common (extractRow)
import Data.Sparse.SpMatrix
import Numeric.LinearAlgebra.Sparse

import qualified Data.Sequence as S
import Data.Bits

type Snapshot = SpMatrix Double
type InteractionNetwork = [Snapshot]

adjacencyMatrix :: Int -> [(Int,Int)] -> Snapshot
adjacencyMatrix numNodes interactions = fromListSM (numNodes,numNodes) $ map (\(x, y) -> (x,y,1)) interactions

interactionNetwork :: Int -> [[(Int, Int)]] -> InteractionNetwork
interactionNetwork size = map (adjacencyMatrix size)

numAgents :: InteractionNetwork -> Int
numAgents = fst . dimSM . head

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
          where diagonals (_, d, _) = toListSM d

-- | Compute the characteristic temporal path length of the graph.
ctpl :: InteractionNetwork -> Double
ctpl network = total / fromIntegral (n*(n-1))
  where n = numAgents network
        total = fromIntegral $ sum [ sum . extantPaths . bfs network $ u | u <- [0..(n-1)] ]

        extantPaths :: [(Int, Maybe Int)] -> [Int]
        extantPaths = foldr (\(_, path) acc -> case path of
                                                 Just x  -> x:acc
                                                 Nothing -> acc) []


-- | Geeralized BFS for time varying graphs What about unreachable
--   nodes? This is simply avoided by Tang et al by using the temporal
--   efficiency. When they compute the CTPL, it appears that they
--   leave out any nodes which are unreachable (although this is unclear).
bfs :: InteractionNetwork -> Int -> [(Int, Maybe Int)]
bfs net source = bfs' 1 net (bit source) (S.singleton source) [(source, Just 0)] 
  where bfs' :: Int -> InteractionNetwork -> Integer -> S.Seq Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
        bfs' t net@(s:future) !visited queue tpLengths =
          let front S.:< rest = S.viewl queue
              frontier   = explore visited s front -- get all adjacent nodes to front that have not already been visited
              visited'   = visited .|. (foldr (\x acc -> setBit acc x) zeroBits frontier)
              queue'     = rest S.>< (S.fromList $ frontier ++ [front])
              tpLengths' = map (\x -> (x, Just t)) frontier
          in
            if front /= source -- source acts as a sentinel, marking the "begining" of the queue
            then bfs' t net visited' queue' tpLengths'
            else bfs' (t+1) future visited' queue' tpLengths'
        bfs' _ [] visited _ tpLengths = undefined -- TODO: Fill in unreachable nodes with (n, Nothing)

        -- | Get all nodes adjacent to n that have not beed discovered yet.
        explore :: Integer -> Snapshot -> Int -> [Int]
        explore visited s = filter (not . testBit visited) . map fst . toListSV . extractRow s
                                
               