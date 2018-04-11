module Main where

import Model
import System.Random

import Control.Monad

teleport :: Double -> (Point -> Double -> Double -> Double -> Point)
teleport p = (\c p' x y -> if p' < p then (x, y) else c)

-- angle of the ray from the origin to the point, measured from the x-axis
direction :: Point -> Double
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
  let rs             = map (randomRs (-arenaSize,arenaSize::Double)) gens
      teleportations = (zipWith positionStrategy3 rs (repeat (teleport p)))
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) teleportations

main :: IO ()
main = do
  model <- teleportationModel arenaSize commRange agentSpeed 0.001 100
  mapM_ print $ map (mapAgents position) (take 1000 $ iterate (stepModel 1.0) model)
  where arenaSize  = 1000
        commRange  = 1
        agentSpeed = 1