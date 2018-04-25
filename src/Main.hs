module Main where

import Model
import InteractionNetwork

import System.Random
import System.Environment

import Numeric.LinearAlgebra.Sparse (prd)

import Control.Monad

teleport :: Double -> Double -> Double -> (Point -> Double -> Double -> Double -> Point)
teleport p w h = (\c p' x y -> if p' < p then (w*(x - 0.5), h*(y - 0.5)) else c)

brownian :: Double -> Double -> Double
brownian _ = id

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
      teleportations = (zipWith positionStrategy3 rs (repeat (teleport p a a)))
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) teleportations
    where a = 2 * arenaSize

brownianModel :: Double -> Double -> Double -> Int -> IO Model
brownianModel arenaSize commRange agentSpeed numAgents = do
  gens <- replicateM numAgents newStdGen
  let rs = map (randomRs (0, 2*pi)) gens
      turns = zipWith headingStrategy rs (repeat brownian)
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) turns

getInteractionNetwork :: [Model] -> InteractionNetwork
getInteractionNetwork ts@(m:_) = interactionNetwork (numAgents m) $ map getInteractions ts

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

  -- mapM_ prd $ getInteractionNetwork (runModel 1.0 1000 model)
  let inet = drop 1000 $ getInteractionNetwork (runModel 1.0 1500 model)
  putStrLn $ "TCC:  " ++ (show $ tcc inet)
  putStrLn $ "CTPL: " ++ (show $ ctpl inet)
  putStrLn $ "TGE:  " ++ (show $ tge inet)
  where commRange  = 5 -- From the Tang paper
        agentSpeed = 1