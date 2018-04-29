{-# LANGUAGE BangPatterns #-}
module InteractionNetwork
  ( adjacencyMatrix
  , interactionNetwork
  , tcc
  , ctpl
  , tge
  , allPairsBFS
  , spectralRadius
  , spectralRadius'
  , centralities
  , oneHopCentralities
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

-- NOTE: the adjacency matrix is symmetric, symmetric matricies have real-valued eigenvalues.
spectralRadius :: InteractionNetwork -> Double
spectralRadius network = foldr (\s acc -> let rho = maxElement . cmap abs . eigenvaluesSH . trustSym $ toDense s in
                                   if rho > acc then rho else acc) 0.0 (graph network)

-- minimum sr over all timesteps
spectralRadius' :: InteractionNetwork -> Double
spectralRadius' network =
  minimum $ map (maxElement . cmap abs . eigenvaluesSH . trustSym . toDense) (graph network)

oneHopCommunicability :: InteractionNetwork -> Matrix Double
oneHopCommunicability network =
  communicability' g (ident n)
  where a = 0.1
        n = numNodes network
        g = graph network

        communicability' []       q = q
        communicability' (s:rest) q =
          -- by replacing (I - aA) with (I + aA) we can enforce only
          -- on hop per time step (apparently)
          let q' = q <> ((ident n) + (a*(toDense s))) in
            communicability' rest $ q' / scalar (norm_2 q')

-- | Compute the normalized communicability matrix.
communicability :: InteractionNetwork -> Matrix Double
communicability network =
  communicability' g (ident n)
  -- take a < 1 per Grindrod 2010
  -- a = 0.1 seems sufficient for teleportation, brownian, and crw models
  where a = 0.1 -- XXX: a < 1/max (ρ(A[k])) for k in 1..length g
        n = numNodes network
        g = graph network

        communicability' []       q = q
        communicability' (s:rest) q =
          let q' = q <> (inv $ (ident n) - (a*(toDense s))) in
            communicability' rest $ q' / scalar (norm_2 q')

centralities :: InteractionNetwork -> (Vector Double, Vector Double)
centralities network = (broadcastCentralities network q, receiveCentralities network q)
  where q = communicability network

oneHopCentralities :: InteractionNetwork -> (Vector Double, Vector Double)
oneHopCentralities network = (broadcastCentralities network q, receiveCentralities network q)
  where q = oneHopCommunicability network

broadcastCentralities :: InteractionNetwork -> Matrix Double -> Vector Double
broadcastCentralities network q =
  q #> (vector $ replicate (numNodes network) 1)

receiveCentralities :: InteractionNetwork -> Matrix Double -> Vector Double
receiveCentralities network q =
  (vector $ replicate (numNodes network) 1) <# q

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
