module Main where

import Model
import InteractionNetwork

import Numeric.LinearAlgebra

import System.Environment

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
               teleportationModel arenaSize commRange agentSpeed p numAgents identityUpdate
             "uniform" -> do
               uniformModel arenaSize commRange agentSpeed numAgents identityUpdate
             "crw"      -> do
               let stdDev = read (head rest)
               crwModel arenaSize commRange agentSpeed stdDev numAgents identityUpdate
             "levy"     -> do
               -- the extra parameter on the command line is 0 < alpha
               -- <= 2 for alpha = 2 the PDF becomes gaussian, for
               -- alpha = 1 The PDF is a Cauchy distribution.
               let [m,r] = rest
                   mu = (-1) - (read m)
                   maxJump = read r
               levyModel arenaSize commRange agentSpeed mu 1 maxJump numAgents identityUpdate

  let inet = getInteractionNetworkAfter 1000 (runModel 1.0 1500 model)
      temporalBST = allPairsBFS inet
--  putStrLn $ "TCC:  " ++ (show $ tcc inet)
--  putStrLn $ "CTPL: " ++ (show $ ctpl inet temporalBST)
--  putStrLn $ "TGE:  " ++ (show $ tge inet temporalBST)
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