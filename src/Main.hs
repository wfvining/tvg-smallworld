module Main where

import Model
import System.Random

teleport :: Double -> (Point -> Double -> Double -> Double -> Point)
teleport p = (\c p' x y -> if p' < p then (x, y) else c)

main :: IO ()
main = do
  gen <- newStdGen
  let rs = randomRs (-100,100) gen
  let model = newModel 100 5 2.0 (\i -> ((0.0+(fromIntegral i),0.0), (fromIntegral i))) [positionStrategy3 rs (teleport 0.05)]
      positions = mapAgents position model
  mapM_ print positions
  putStrLn " --- "
  mapM_ print $ map (mapAgents position) (take 100 $ iterate (stepModel 1.0) model)
