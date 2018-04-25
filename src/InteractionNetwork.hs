{-# LANGUAGE BangPatterns #-}
module InteractionNetwork
  ( adjacencyMatrix
  , interactionNetwork
  , tcc
  , ctpl
  , tge
  , allPairsBFS
  , InteractionNetwork(..)
  ) where

import Data.Sparse.Common (extractRow)
import Data.Sparse.SpMatrix
import Data.Sparse.SpVector
import Numeric.LinearAlgebra.Sparse

import qualified Data.Sequence as S
import Data.Bits
import Data.Maybe

type Snapshot = SpMatrix Double
type InteractionNetwork = [Snapshot]

adjacencyMatrix :: Int -> [(Int,Int)] -> Snapshot
adjacencyMatrix numNodes interactions = fromListSM (numNodes,numNodes) $ map (\(x, y) -> (x,y,1)) interactions

interactionNetwork :: Int -> [[(Int, Int)]] -> InteractionNetwork
interactionNetwork size = map (adjacencyMatrix size)

numAgents :: InteractionNetwork -> Int
numAgents = fst . dimSM . head

allPairsBFS :: InteractionNetwork -> [[(Int, Maybe Int)]]
allPairsBFS network = [ bfs network u | u <- [0..(n-1)] ]
  where n = numAgents network

-- | Compute the temporal correlation coefficient
--
-- Should be 1 if every snapshot is the same
--
-- Follows the equation from Clausset paper, accounting for vertices
-- that don't have any edges at all.
tcc :: InteractionNetwork -> Double
tcc net = (sum $ foldr (\xs acc -> zipWith (+) xs acc) (repeat 0.0) [ cit t t' | (t,t') <- zip net (tail net) ])
          / (fromIntegral $ (timesteps-1) * numAgents net)
  where timesteps = length net
        cit :: Snapshot -> Snapshot -> [Double]
        cit t t' = let rows  = toRowsL t
                       rows' = toRowsL t'
                   in
                     zipWith cij rows rows'
        cij :: SpVector Double -> SpVector Double -> Double
        cij r r'
          | nnz r == 0 || nnz r' == 0 = 1
          | otherwise = (dot' (toListSV r) (toListSV r') 0.0) / (sqrt $ (sum r) * (sum r'))

        dot' :: [(Int, Double)] -> [(Int, Double)] -> Double -> Double
        dot' [] _ acc = acc
        dot' _ [] acc = acc
        dot' xs@((x,vx):restx) ys@((y,vy):resty) acc
          | x == y = dot' restx resty (vx*vy + acc)
          | x < y  = dot' restx ys acc
          | x > y  = dot' xs resty acc

-- | Compute the characteristic temporal path length of the graph.
--
-- Any nodes that do not have a temporal path are excluded from the
-- average.
ctpl :: InteractionNetwork -> [[(Int, Maybe Int)]] -> Double
ctpl network bst = total / (fromIntegral . length $ pathLengths)
  where pathLengths = map (\(_, (Just x)) -> x) . filter (\(_, x) -> case x of
                                                                       Just 0 -> False
                                                                       Just x -> True
                                                                       Nothing -> False) $
                      concat bst
        total = fromIntegral $ sum pathLengths

-- | Temporal Global Efficiency
tge :: InteractionNetwork -> [[(Int, Maybe Int)]] -> Double
tge network bst = total / fromIntegral (n*(n-1))
  where n     = numAgents network
        total = sum $ map rpSum bst

        rpSum :: [(Int, Maybe Int)] -> Double
        rpSum = foldr (\(_, path) !acc -> case path of
                                           Just 0 -> acc -- ignore self paths
                                           Just x -> (1/fromIntegral x) + acc
                                           Nothing -> acc) 0.0

-- | Geeralized BFS for time varying graphs. Returns the length of the
-- shortest temporal path from a source node to all other node in the
-- network.
--
-- Any node `x' that is unreachable will apear in the result as (x,
-- Nothing).
bfs :: InteractionNetwork -> Int -> [(Int, Maybe Int)]
bfs net source = bfs' 1 net (bit source) (S.singleton source) [(source, Just 0)] 
  where bfs' :: Int -> InteractionNetwork -> Integer -> S.Seq Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
        bfs' t net@(s:future) !visited queue tpLengths =
          let front S.:< rest = S.viewl queue
              frontier   = explore visited s front -- get all adjacent nodes to front that have not already been visited
              visited'   = visited .|. (foldr (\x acc -> setBit acc x) zeroBits frontier)
              queue'     = rest S.>< (S.fromList $ frontier ++ [front])
              tpLengths' = tpLengths ++ map (\x -> (x, Just t)) frontier
          in
            if front /= source -- source acts as a sentinel, marking the "begining" of the queue
            then bfs' t net visited' queue' tpLengths'
            else bfs' (t+1) future visited' queue' tpLengths'
        bfs' _ [] visited _ tpLengths = [ (x, Nothing) | x <- [0..((numAgents net) - 1)], not $ testBit visited x ] ++ tpLengths

        -- | Get all nodes adjacent to n that have not beed discovered yet.
        explore :: Integer -> Snapshot -> Int -> [Int]
        explore visited s = filter (not . testBit visited) . map fst . toListSV . extractRow s
