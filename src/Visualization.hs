module Main where

import Model
import System.Environment

import Graphics.Gloss

vizScale :: Int
vizScale = 4

modelToPicture :: Model -> Picture
modelToPicture m =
  let agentStatus = mapAgents (\a -> (position a, state a)) m in
    pictures $ [ let pf = toFloatPoint p
                     pf' = toFloatPoint p' in
                   color (greyN 0.5) $ line [pf,pf'] | (p, _) <- agentStatus
                                               , (p', _) <- agentStatus
                                               , distance p p' < (range m)
                                               , p /= p' ]
               ++ [ translate (-s/2) ((s/2) + 5) $ scale 0.2 0.2  $ color black $ text $ "time: " ++ show (time m), color (greyN 0.7) $ circleSolid (0.5 * fromIntegral vizScale) ]
               ++ [ translate (realToFrac $ x*(fromIntegral vizScale)) (realToFrac $ y*(fromIntegral vizScale))
                    $ color (toColor st) $ circleSolid 2 | ((x, y), st) <- agentStatus ]
               ++ [ translate (realToFrac $ x*(fromIntegral vizScale)) (realToFrac $ y*(fromIntegral vizScale))
                    $ color black $ circle 2 | ((x, y), _) <- agentStatus ]
               ++ [ color black $ lineLoop $ rectanglePath s s ]
               ++ [ translate (-s/2) ((-s/2) - 25) $ scale 0.2 0.2 $ color black $ text $ "density: " ++ show (density m) ]
  where toFloatPoint (x, y) = (realToFrac (x * fromIntegral vizScale), realToFrac (y * fromIntegral vizScale))
        s = 2 * (realToFrac $ (fromIntegral vizScale) * (size m))
        toColor Black = black
        toColor White = white

main :: IO ()
main = do
  (motionType:n:arena:rest) <- getArgs
  let arenaSize = read arena
      numAgents = read n

  initialModel <- case motionType of
                    "teleport" -> do
                      let p = read (head rest)
                      teleportationModel arenaSize commRange agentSpeed p numAgents simpleMajority
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
                      levyModel arenaSize commRange agentSpeed mu 1 maxJump numAgents simpleMajority

  -- (last $ runModel 1.0 500 initialModel)
  simulate window background fps (last $ runModel 1.0 400 initialModel) modelToPicture update
  where speedup    = 20
        update _ s m = if density m > 0 && density m < 1 then stepModel (realToFrac $ speedup * s) m else m
        window     = InWindow "INet" (60 + (vizScale * 100), 60 + (vizScale * 100)) (10,10)
        fps        = 60
        background = white
        commRange  = 5
        agentSpeed = 1.0
  