module Main where

import Model
import InteractionNetwork
import Config

import Numeric.LinearAlgebra

import System.Environment

getInteractionNetwork :: [Model] -> InteractionNetwork
getInteractionNetwork ts@(m:_) = interactionNetwork (numAgents m) $ map getInteractions ts

getInteractionNetworkAfter :: Int -> [Model] -> InteractionNetwork
getInteractionNetworkAfter k ts = getInteractionNetwork $ drop k ts

main :: IO ()
main = do
  [configFile,dataDir] <- getArgs
  config <- loadConfig configFile

  let model = newModel config
      inet = getInteractionNetworkAfter 1000 (runModel 1.0 1500 model)
      temporalBST = allPairsBFS inet
  putStrLn $ "TCC:  " ++ (show $ tcc inet)
  putStrLn $ "CTPL: " ++ (show $ ctpl inet temporalBST)
  putStrLn $ "TGE:  " ++ (show $ tge inet temporalBST)
  putStrLn $ "1/max ρ(·): " ++ (show $ 1 / (spectralRadius inet))
  putStrLn $ "1/min ρ(·): " ++ (show $ 1 / (spectralRadius' inet))

  -- let cm  = fromColumns . (\(a,b) -> a:b:[]) $ centralities inet
  --     ocm = fromColumns . (\(a,b) -> a:b:[]) $ oneHopCentralities inet

  -- saveMatrix (dataDir ++ "centralities_"++motionType++"-"++n++"-"++arena++fileNameExtra) "%f" cm
  -- saveMatrix (dataDir ++ "oneHopCentralities_"++motionType++"-"++n++"-"++arena++fileNameExtra) "%f" ocm