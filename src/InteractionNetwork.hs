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

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data (mkSparse, nCols)

import qualified Data.Sequence as S
import Data.Bits
import Data.Maybe

type Snapshot = AssocMatrix
data InteractionNetwork = INet { numNodes :: Int , graph :: [Snapshot] }

adjacencyMatrix :: Int -> [(Int,Int)] -> Snapshot
adjacencyMatrix numNodes interactions = [((n, n), 0)] ++ map (\(x, y) -> ((x,y),1)) interactions
  where n = numNodes - 1

interactionNetwork :: Int -> [[(Int, Int)]] -> InteractionNetwork
interactionNetwork size interactions = INet { numNodes = size, graph = map (adjacencyMatrix size) interactions }

allPairsBFS :: InteractionNetwork -> [[(Int, Maybe Int)]]
allPairsBFS network = [ bfs network u | u <- [0..(n-1)] ]
  where n = numNodes network

-- | Compute the temporal correlation coefficient
--
-- Should be 1 if every snapshot is the same
--
-- Follows the equation from Clausset paper, accounting for vertices
-- that don't have any edges at all.
tcc :: InteractionNetwork -> Double
tcc net = (sum $ foldr (\xs acc -> zipWith (+) xs acc) (repeat 0.0) [ cit t t' | (t,t') <- zip g (tail g) ])
          / (fromIntegral $ (timesteps-1) * numNodes net)
  where timesteps = length (graph net)
        g         = graph net
        
        cit :: Snapshot -> Snapshot -> [Double]
        cit t t' = let rows  = toRows $ toDense t
                       rows' = toRows $ toDense t'
                   in
                     zipWith cij rows rows'
        cij :: Vector Double -> Vector Double -> Double
        cij r r' = if sr == 0 || sr' == 0 then 1 else  (r <.> r') / (sqrt $ (sr) * (sr'))
          where sr  = sumElements r
                sr' = sumElements r'

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
  where n     = numNodes network
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
bfs net source = bfs' 1 (graph net) (bit source) (S.singleton source) [(source, Just 0)] 
  where bfs' :: Int -> [Snapshot] -> Integer -> S.Seq Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
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
        bfs' _ [] visited _ tpLengths = [ (x, Nothing) | x <- [0..((numNodes net) - 1)], not $ testBit visited x ] ++ tpLengths

        -- | Get all nodes adjacent to n that have not beed discovered yet.
        explore :: Integer -> Snapshot -> Int -> [Int]
        explore visited s source = map (snd . fst) $ filter (\((x,y),z) -> (z /= 0) && (x == source) && (not $ testBit visited y)) s
