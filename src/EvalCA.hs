module Main where

import Config
import Model

import System.Environment
import System.IO

evalReplica :: Int -> TVGConfig -> [Double]
evalReplica r config =
    let replicaConfig = config { seed = r + seed config }
    in --takeWhile (\currentDensity -> currentDensity /= 0.0 && currentDensity /= 1.0)
           take (maxSteps config)
           $ map density
           $ iterate (stepModel 1.0) (newModel replicaConfig)

saveDensityTimeSeries :: FilePath -> TVGConfig -> [[Double]] -> IO ()
saveDensityTimeSeries dataDir config densities = do
  writeFile tsFile (unlines $ records densities)
      where tsFile = dataDir++ "/" ++ msName ++ (show $ seed config)
            msName = show $ movementStrategy config
            
records :: Show a => [[a]] -> [String]
records ([]:_) = []
records xss = let values = concatMap (\xs -> (show $ head xs) ++ " ") xss
              in values : records (map tail xss)

main :: IO ()
main = do
  [configFile, dataDir] <- getArgs
  config <- loadConfig configFile

  let runs = [ evalReplica i config | i <- [0..(numRepetitions config)-1] ]
             
  saveDensityTimeSeries dataDir config runs
  print $ successRate config runs
      where successRate config runs = let finalDensities = map last runs
                                      in (fromIntegral
                                          $ length (filter
                                                    (==(fromIntegral . round $ initialDensity config))
                                                    finalDensities))
                                             / (fromIntegral $ numRepetitions config)
