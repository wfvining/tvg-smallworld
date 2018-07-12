module Main where

import Config
import Model

import System.Environment

evalReplica :: Int -> TVGConfig -> [Double]
evalReplica r config =
    let replicaConfig = config { seed = r + seed config }
    in takeWhile (\currentDensity -> currentDensity /= 0.0 && currentDensity /= 1.0)
           $ take (maxSteps config)
           $ map density
           $ iterate (stepModel 1.0) (newModel replicaConfig)

main :: IO ()
main = do
  [configFile, dataDir] <- getArgs
  config <- loadConfig configFile

  mapM_ (print . length) [ evalReplica i config | i <- [0..(numRepetitions config)-1] ]
