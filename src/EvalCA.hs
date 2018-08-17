module Main where

import Config
import Model

import System.Environment
import System.IO

evalReplica :: Int -> TVGConfig -> [Double]
evalReplica r config =
    let replicaConfig = config { seed = r + seed config }
        (densities, rest) = span (\currentDensity -> currentDensity /= 0.0 && currentDensity /= 1.0)
                                 $ take (maxSteps config)
                                 $ map density
                                 $ iterate (stepModel 1.0) (newModel replicaConfig)
    in case rest of
      [] -> densities
      (final:_) -> padTo (maxSteps config) (densities ++ [final])
  where padTo :: Int -> [Double] -> [Double]
        padTo n xs = let l = length xs in
                       if l < n
                       then xs ++ (replicate (n-l) (last xs))
                       else xs
                                

saveDensityTimeSeries :: FilePath -> TVGConfig -> [[Double]] -> IO ()
saveDensityTimeSeries dataDir config densities = do
  writeFile tsFile (unlines $ records densities)
      where tsFile = dataDir++ "/" ++ msName ++ (show $ seed config)
            msName = show $ movementStrategy config
            
records :: Show a => [[a]] -> [String]
records ([]:_) = []
records xss = let values = concatMap (\xs -> (show $ head xs) ++ " ") xss
              in values : records (map tail xss)

printPrefix :: TVGConfig -> IO ()
printPrefix TVGConfig{ initialDensity=i, movementStrategy=strat } = do
  putStr $ (show i) ++ " "
  printStrategyPrefix strat
    where printStrategyPrefix Levy{alpha=a,maxStep=m} = putStr $ (show a) ++ " " ++ (show m) ++ " "
          printStrategyPrefix Teleport{pJump = p} = putStr $ (show p) ++ " "
          printStrategyPrefix Homing{ sigma=s, pHome = p} = putStr $ (show s) ++ " " ++ (show p) ++ " "
          printStrategyPrefix CRW{sigma=s} = putStr $ (show s) ++ " "
          printStrategyPrefix Ballistic = return ()

main :: IO ()
main = do
  [configFile, dataDir] <- getArgs
  config <- loadConfig configFile

  let runs = [ evalReplica i config | i <- [0..(numRepetitions config)-1] ]
             
  saveDensityTimeSeries dataDir config runs

  printPrefix config
  print $ successRate config runs
      where successRate config runs = let results = zip (map (fromIntegral . round . head) runs) (map last runs)
                                      in (fromIntegral
                                          $ length (filter (uncurry (==)) results))
                                             / (fromIntegral $ numRepetitions config)
