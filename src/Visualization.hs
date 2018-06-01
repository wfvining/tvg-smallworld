module Main where

import Model
import System.Environment

import Graphics.Gloss

vizScale :: Int
vizScale = 4

modelToPicture :: Model -> Picture
modelToPicture m =
  let agentPositions = mapAgents position m in
    pictures $ [ let pf = toFloatPoint p
                     pf' = toFloatPoint p' in
                   color (greyN 0.5) $ line [pf,pf'] | p <- agentPositions
                                               , p' <- agentPositions
                                               , distance p p' < (range m)
                                               , p /= p' ]
               ++ [ translate (realToFrac $ x*(fromIntegral vizScale)) (realToFrac $ y*(fromIntegral vizScale))
                    $ color black $ circleSolid 2 | (x, y) <- agentPositions]
               ++ [ color black $ lineLoop $ rectanglePath s s ] ++ [ translate (-s/2) ((s/2) + 5) $ scale 0.2 0.2  $ color black $ text $ show (time m), color (greyN 0.7) $ circleSolid (0.5 * fromIntegral vizScale) ]
  where toFloatPoint (x, y) = (realToFrac (x * fromIntegral vizScale), realToFrac (y * fromIntegral vizScale))
        s = 2 * (realToFrac $ (fromIntegral vizScale) * (size m))

main :: IO ()
main = do
  (motionType:n:arena:rest) <- getArgs
  let arenaSize = read arena
      numAgents = read n

  initialModel <- case motionType of
                    "teleport" -> do
                      let p = read (head rest)
                      teleportationModel arenaSize commRange agentSpeed p numAgents identityUpdate
                    "uniform" -> do
                      uniformModel arenaSize commRange agentSpeed numAgents identityUpdate
                    "crw"      -> do
                      let stdDev = read (head rest)
                      crwModel arenaSize commRange agentSpeed stdDev numAgents identityUpdate
                    "home"     -> do
                      let stdDev = read (head rest)
                          p = read (head $ tail rest)
                      homingModel arenaSize commRange agentSpeed p stdDev numAgents simpleMajority
                    "levy"     -> do
                      -- the extra parameter on the command line is 0 < alpha
                      -- <= 2 for alpha = 2 the PDF becomes gaussian, for
                      -- alpha = 1 The PDF is a Cauchy distribution.
                      let [m,r] = rest
                          mu = (-1) - (read m)
                          maxJump = read r
                      levyModel arenaSize commRange agentSpeed mu 1 maxJump numAgents identityUpdate

  simulate window background fps initialModel modelToPicture update
  where speedup    = 5
        update _ s = stepModel (realToFrac $ speedup * s)
        window     = InWindow "INet" (60 + (vizScale * 100), 60 + (vizScale * 100)) (10,10)
        fps        = 60
        background = white
        commRange  = 5
        agentSpeed = 1.0
  