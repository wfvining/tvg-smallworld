module Main where

import Model
import Config
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
  [configFile] <- getArgs
  config <- loadConfig configFile

  let initialModel = newModel config (initSquare . ceiling . sqrt . fromIntegral $ nAgents config) identityUpdate
  
  -- (last $ runModel 1.0 500 initialModel)
  simulate window background fps (last $ runModel 1.0 400 initialModel) modelToPicture update
  where speedup    = 20
        update _ s m = if density m > 0 && density m < 1 then stepModel (realToFrac $ speedup * s) m else m
        window     = InWindow "INet" (60 + (vizScale * 100), 60 + (vizScale * 100)) (10,10)
        fps        = 60
        background = white
  