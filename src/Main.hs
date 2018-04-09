module Main where

import Model

main :: IO ()
main = do
  let model = newModel 100 5 2.0 (\i -> ((0.0+(fromIntegral i),0.0), (fromIntegral i))) (replicate 10 identityStrategy)
      positions = mapAgents model position
  mapM_ print positions
  putStrLn " --- "
  mapM_ print $ mapAgents (iterate stepModel 1.0)
