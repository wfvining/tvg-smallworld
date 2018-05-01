module Main where

import Model
import InteractionNetwork

import Numeric.LinearAlgebra

import System.Random
import System.Environment

import Control.Monad

import Data.Random.Normal

teleport :: Double -> Double -> Double -> (Point -> Double -> Double -> Double -> Point)
teleport p w h = (\c p' x y -> if p' < p then (w*(x - 0.5), h*(y - 0.5)) else c)

brownian :: Double -> Double -> Double
brownian _ = id

crw :: Double -> Double -> Double
crw h t = let x = h + t in
  if x >= 0 && x <= 2*pi then x else x - 2*pi*(fromIntegral $ floor (x / (2*pi)))

levyWalk :: [Double] -> [Double] -> MovementStrategy
levyWalk xs ys = Comp (velocityStrategy xs (\x y -> y)) (headingStrategy ys brownian)

powerLaw :: Double -> Double -> Double -> Double -> Double
powerLaw min max n y = ((((max ** (n+1)) - (min ** (n+1))) * y) + (min ** (n+1))) ** (1/(n+1))

-- angle of the ray from the origin to the point, measured from the x-axis
direction :: Point -> Double
-- define the direction for 0,0 to be 0
direction (0, 0) = 0
direction (x, y) =
  if x < 0
  then pi + theta
  else if y < 0
       then (2*pi) + theta
       else theta
  where theta = atan (y / x)

initRectangle :: Int -> Int -> Initializer
initRectangle dimensionX dimensionY =
  (\agentID -> let position@(x,y) = (fromIntegral $ upperLeftX + (agentID `div` dimensionX),
                                     fromIntegral $ upperLeftY - (agentID `rem` dimensionX)) in
                 (position, direction position))
  where upperLeftX = -(dimensionX `div` 2)
        upperLeftY = dimensionY `div` 2

initSquare :: Int -> Initializer
initSquare dimension = initRectangle dimension dimension

teleportationModel :: Double -> Double -> Double -> Double -> Int -> IO Model
teleportationModel arenaSize commRange agentSpeed p numAgents = do
  gens <- replicateM numAgents newStdGen
  let rs             = map (randomRs (1,0::Double)) gens
      teleportations = (zipWith positionStrategy3 rs (repeat (teleport p arenaSize arenaSize)))
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) teleportations

brownianModel :: Double -> Double -> Double -> Int -> IO Model
brownianModel arenaSize commRange agentSpeed numAgents = do
  gens <- replicateM numAgents newStdGen
  let rs = map (randomRs (0, 2*pi)) gens
      turns = zipWith headingStrategy rs (repeat brownian)
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) turns

crwModel :: Double -> Double -> Double -> Double -> Int -> IO Model
crwModel arenaSize commRange agentSpeed sigma numAgents = do
   rs <- replicateM numAgents (normalsIO' (0.0, sigma))
   let turns = zipWith headingStrategy rs (repeat crw)
   return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) turns

levyModel :: Double -> Double -> Double -> Double -> Double -> Double -> Int -> IO Model
levyModel arenaSize commRange agentSpeed mu minStep maxStep numAgents = do
  gens  <- replicateM numAgents newStdGen
  gens' <- replicateM numAgents newStdGen
  let rs = map (randomRs (0, 2*pi)) gens
      rs' = map (map (powerLaw minStep maxStep mu)) $ map (randomRs (0,1)) gens'
      walks = zipWith levyWalk rs' rs
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) walks

getInteractionNetwork :: [Model] -> InteractionNetwork
getInteractionNetwork ts@(m:_) = interactionNetwork (numAgents m) $ map getInteractions ts

getInteractionNetworkAfter :: Int -> [Model] -> InteractionNetwork
getInteractionNetworkAfter k ts = getInteractionNetwork $ drop k ts

main :: IO ()
main = do
  (motionType:n:arena:rest) <- getArgs
  let arenaSize = read arena
      numAgents = read n

  model <- case motionType of
             "teleport" -> do
               let p = read (head rest)
               teleportationModel arenaSize commRange agentSpeed p numAgents
             "brownian" -> do
               brownianModel arenaSize commRange agentSpeed numAgents
             "crw"      -> do
               let stdDev = read (head rest)
               crwModel arenaSize commRange agentSpeed stdDev numAgents
             "levy"     -> do
               -- the extra parameter on the command line is 0 < alpha
               -- <= 2 for alpha = 2 the PDF becomes gaussian, for
               -- alpha = 1 The PDF is a Cauchy distribution.
               let mu = (-1) - (read (head rest))
               levyModel arenaSize commRange agentSpeed mu 1 5 numAgents

  let inet = getInteractionNetworkAfter 1000 (runModel 1.0 1500 model)
      temporalBST = allPairsBFS inet
  putStrLn $ "TCC:  " ++ (show $ tcc inet)
  putStrLn $ "CTPL: " ++ (show $ ctpl inet temporalBST)
  putStrLn $ "TGE:  " ++ (show $ tge inet temporalBST)
  putStrLn $ "1/max ρ(·): " ++ (show $ 1 / (spectralRadius inet))
  putStrLn $ "1/min ρ(·): " ++ (show $ 1 / (spectralRadius' inet))

  let fileNameExtra = case motionType of
                        "teleport" -> "-" ++ (head rest)
                        "crw"      -> "-" ++ (head rest)
                        "levy"     -> "-" ++ (head rest)
                        "brownian" -> ""

  let cm  = fromColumns . (\(a,b) -> a:b:[]) $ centralities inet
      ocm = fromColumns . (\(a,b) -> a:b:[]) $ oneHopCentralities inet

  saveMatrix ("centralities_"++motionType++"-"++n++"-"++arena++fileNameExtra) "%f" cm
  saveMatrix ("oneHopCentralities_"++motionType++"-"++n++"-"++arena++fileNameExtra) "%f" ocm

  where commRange  = 5 -- From the Tang paper
        agentSpeed = 1