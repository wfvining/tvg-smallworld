module Main where

import Model
import System.Environment

import Graphics.Gloss

vizScale :: Int
vizScale = 4

modelToPicture :: Model -> Picture
modelToPicture m =
  let agentInfo = mapAgents (\a -> (position a, agentColor a)) m in
    pictures $ [ let pf = toFloatPoint p
                     pf' = toFloatPoint p' in
                   color (greyN 0.3) $ line [pf,pf'] | (p, _) <- agentInfo
                                               , (p', _) <- agentInfo
                                               , distance p p' < (range m)
                                               , p /= p' ]
               ++ [ translate (realToFrac $ x*(fromIntegral vizScale)) (realToFrac $ y*(fromIntegral vizScale))
                    $ c $ circleSolid 3 | ((x, y), c) <- agentInfo]
               ++ [ color white $ lineLoop $ rectanglePath s s ] ++ [ translate (-s/2) ((s/2) + 5) $ scale 0.2 0.2  $ color white $ text $ show (time m) ]
  where toFloatPoint (x, y) = (realToFrac (x * fromIntegral vizScale), realToFrac (y * fromIntegral vizScale))
        s = 2 * (realToFrac $ (fromIntegral vizScale) * (size m))
        agentColor a = color $ case state a of
                                 White -> white
                                 Red   -> red
                                 Green -> green
                                 Blue  -> light blue

main :: IO ()
main = do
  (motionType:n:arena:rest) <- getArgs
  let arenaSize = read arena
      numAgents = read n

  initialModel <- case motionType of
                    "teleport" -> do
                      let p = read (head rest)
                      teleportationModel arenaSize commRange agentSpeed p numAgents
                    "brownian" -> do
                      brownianModel arenaSize commRange agentSpeed numAgents
                    "crw"      -> do
                      let stdDev = read (head rest)
                      crwModel arenaSize commRange agentSpeed stdDev numAgents
                    "levy"     -> do
                      -- the extra parameter on the command line is 0 < alpha
                      -- <= 2 for alpha = 2 the PDF becomes gaussian, for
                      -- alpha = 1 The PDF is a Cauchy distribution.
                      let [m,r] = rest
                          mu = (-1) - (read m)
                          maxJump = read r
                      levyModel arenaSize commRange agentSpeed mu 1 maxJump numAgents

  simulate window background fps initialModel modelToPicture update
  where speedup    = 2
        update _ s = stepModel (realToFrac $ speedup * s)
        window     = InWindow "INet" (60 + (vizScale * 100), 60 + (vizScale * 100)) (10,10)
        fps        = 60
        background = black
        commRange  = 5
        agentSpeed = 1.0
  