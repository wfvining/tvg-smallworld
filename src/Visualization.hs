module Main where

import Model
import System.Environment

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

modelToPicture :: Double -> Model -> Picture
modelToPicture vizScale m =
  let agentInfo = mapAgents (\a -> (position a, agentColor a)) m in
    pictures $ [ let pf = toFloatPoint p
                     pf' = toFloatPoint p' in
                   color (greyN 0.3) $ line [pf,pf'] | (p, _) <- agentInfo
                                               , (p', _) <- agentInfo
                                               , distance p p' < (range m)
                                               , p /= p' ]
               ++ [ translate (realToFrac $ x*vizScale) (realToFrac $ y*vizScale)
                    $ c $ circleSolid agentSize | ((x, y), c) <- agentInfo]
               ++ [ color white $ lineLoop $ rectanglePath s s ]
               ++ [ translate (-s/2) ((s/2) + 5) $ scale 0.2 0.2  $ color white $ text $ show (time m) ]
  where toFloatPoint (x, y) = (realToFrac (x * vizScale), realToFrac (y * vizScale))
        s = 2 * (realToFrac $ vizScale * (size m))
        agentSize    = realToFrac $ if vizScale > 1 then 2 else vizScale * 2
        agentColor a = color $ case state a of
                                 White -> white
                                 Red   -> red
                                 Green -> green
                                 Blue  -> light blue

closest :: Model -> (Float, Float) -> Int
closest m (x,y) = agentID $ foldr (\a acc -> if distance (position a) p < distance (position acc) p then a else acc) a' $ agents m
  where a' = Agent { position = (100000, 100000)
                   , agentID = undefined
                   , state = undefined
                   , update = undefined
                   , heading = undefined
                   , speed = undefined }
        p = (realToFrac x, realToFrac y)

main :: IO ()
main = do
  (motionType:n:arena:rest) <- getArgs
  let arenaSize = read arena
      numAgents = read n
      vizScale  = (fromIntegral windowSize) /  arenaSize
      window    = InWindow "INet" (60 + windowSize, 60 + windowSize) (10,10)

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

  play window background fps (last $ runModel 1.0 1000 initialModel) (modelToPicture vizScale) (eventHandler $ realToFrac vizScale) update
  where speedup    = 4
        update s   = stepModel (realToFrac $ speedup * s)
        fps        = 60
        background = black
        commRange  = 5
        agentSpeed = 1.0
        eventHandler vizScale (EventKey (Char 'r') _ _ (x, y)) world = setState world Red (closest world (x / vizScale, y / vizScale))
        eventHandler vizScale (EventKey (Char 'g') _ _ _) world = setState world Green 0
        eventHandler vizScale (EventKey (Char 'b') _ _ _) world = setState world Blue 0
        eventHandler vizScale (EventKey (Char 'w') _ _ _) world = setState world White 0
        eventHandler _ _ w = w
        windowSize = 800
  