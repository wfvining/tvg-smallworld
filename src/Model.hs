{-# LANGUAGE BangPatterns #-}
module Model
  ( Agent(..)
  , Model(..)
  , Initializer(..)
  , Point(..)
  , MovementRule(..)
  , UpdateRule(..)
  , newModel
  , mapAgents
  , getInteractions
  , stepModel
  , runModel
  , levyModel
  , crwModel
  , homingModel
  , uniformModel
  , teleportationModel
  , distance
  , identityUpdate
  ) where

import Control.Monad
import System.Random
import Data.Random.Normal
import Data.Maybe

type Point = (Double, Double)

type Initializer = (Int -> (Point, Double))

data AgentState = Black | White deriving Eq

type MovementRule = Agent -> Agent
type UpdateRule   = [AgentState] -> Agent -> Agent

data Agent = Agent { agentID  :: !Int
                   , rng      :: !StdGen
                   , position :: Point
                   , speed    :: Double
                   , heading  :: Double
                   , state    :: AgentState
                   , updatePredicate :: Maybe (Agent -> Bool)
                   }

instance Eq Agent where
  a1 == a2 = agentID a1 == agentID a2

data Model = Model { agents     :: [Agent] -- list of agents
                   , size       :: !Double  -- bounds of the arena
                   , range      :: !Double  -- communication range of the agents
                   , numAgents  :: !Int  -- number of agents
                   , time       :: Double
                   , nextUpdate :: Double
                   , update     :: !UpdateRule
                   , move       :: !MovementRule
                   }

identityUpdate :: UpdateRule
identityUpdate _ a = a

newModel :: Double
         -> Double
         -> Double
         -> Int
         -> Initializer
         -> UpdateRule
         -> MovementRule
         -> StdGen
         -> Model
newModel size range speed n init updateRule movementRule gen =
  Model { agents     = makeAgents speed init n gen
        , size       = size/2
        , range      = range
        , numAgents  = n
        , time       = 0
        , nextUpdate = 0
        , move       = movementRule
        , update     = updateRule
        }

makeAgents :: Double
           -> Initializer
           -> Int
           -> StdGen
           -> [Agent]
makeAgents speed init n gen =
  [ let (position, heading) = init i in Agent { agentID = i
                                              , position = position
                                              , heading = heading
                                              , rng = gen
                                              , speed = speed
                                              , state = Black
                                              , updatePredicate = Nothing }
  | (i, gen) <- zip [0..(n-1)] (map fst . iterate (split . snd) $ split gen) ]

teleport :: Double -> Double -> Double -> (Point -> Double -> Double -> Double -> Point)
teleport p w h = (\c p' x y -> if p' < p then (w*(x - 0.5), h*(y - 0.5)) else c)

brownian :: Double -> Double -> Double
brownian _ = id

crw :: Double -> Double -> Double
crw h t = let x = h + t in
  if x >= 0 && x <= 2*pi then x else x - 2*pi*(fromIntegral $ floor (x / (2*pi)))

powerLaw :: Double -> Double -> Double -> Double -> Double
powerLaw min max n y = ((((max ** (n+1)) - (min ** (n+1))) * y) + (min ** (n+1))) ** (1/(n+1))

-- angle of the ray from the origin to the point, measured from the x-axis
direction :: Point -> Double
-- define the direction for 0,0 to be 0
direction (0, 0) = 0
direction (x, y) =
  if x < 0
  then pi + theta
  else if y < 0
       then (2*pi) + theta
       else theta
  where theta = atan (y / x)

initRectangle :: Int -> Int -> Initializer
initRectangle dimensionX dimensionY =
  (\agentID -> let position@(x,y) = (fromIntegral $ upperLeftX + (agentID `div` dimensionX),
                                     fromIntegral $ upperLeftY - (agentID `rem` dimensionX)) in
                 (position, direction position))
  where upperLeftX = -(dimensionX `div` 2)
        upperLeftY = dimensionY `div` 2

initSquare :: Int -> Initializer
initSquare dimension = initRectangle dimension dimension

-- super not efficient
initCenter :: [Double] -> Initializer
initCenter xs = (\aid -> ((0,0), xs !! aid))

teleportationModel :: Double -> Double -> Double -> Double -> Int -> UpdateRule -> IO Model
teleportationModel arenaSize commRange agentSpeed p numAgents update = do
  g <- newStdGen
  let teleportation  = (\a ->
                          let (p', gen) = randomR (0,1) $ rng a
                          in if p' < p
                             then let (x, gen')  = randomR (0, 1) gen
                                      (y, gen'') = randomR (0, 1) gen'
                                  in a { rng = gen''
                                       , position = (arenaSize  * (x - 0.5), arenaSize * (y - 0.5))
                                       }
                             else a { rng = gen })
      s = ceiling . sqrt $ fromIntegral numAgents
  return $ newModel arenaSize commRange agentSpeed numAgents (initSquare s) update teleportation g

uniformModel :: Double -> Double -> Double -> Int -> UpdateRule -> IO Model
uniformModel arenaSize commRange agentSpeed numAgents update = do
  g <- newStdGen
  let turn = (\a ->
                let (newHeading, gen) = randomR (0, 2*pi) $ rng a
                in a { rng = gen, heading = newHeading })
      s = ceiling . sqrt $ fromIntegral numAgents
  return $ newModel arenaSize commRange agentSpeed numAgents (initSquare s) update turn g

crwModel :: Double -> Double -> Double -> Double -> Int -> UpdateRule -> IO Model
crwModel arenaSize commRange agentSpeed sigma numAgents update = do
  g <- newStdGen
  let turn = (\a ->
                let (newHeading, gen) = normal' ((heading a), sigma*pi) (rng a)
                in a { rng = gen, heading = newHeading })
      s = ceiling . sqrt $ fromIntegral numAgents
  return $ newModel arenaSize commRange agentSpeed numAgents (initSquare s) update turn g


homingModel :: Double -> Double -> Double -> Double -> Double -> Int -> UpdateRule -> IO Model
homingModel arenaSize commRange agentSpeed p sigma numAgents update = do
  g <- newStdGen
  return $ newModel arenaSize commRange agentSpeed numAgents (initSquare s) update movement g
  where movement :: Agent -> Agent
        movement a =
          if shouldUpdate a
          then -- go home
            let (p', gen) = randomR (0,1) (rng a) in
              if p' < p
              then
                a { rng = gen
                  , updatePredicate = Just isHome
                  , heading = (2*pi) - (pi - (direction (position a)))
                  }
              else -- just do a random walk
                let (newHeading, gen') = if isJust $ updatePredicate a
                                         then randomR (0, 2*pi) gen
                                         else normal' ((heading a), sigma*pi) gen in
                  a { rng = gen'
                    , heading = newHeading
                    , updatePredicate = Nothing }
          else a

        s = ceiling . sqrt $ fromIntegral numAgents
        
        isHome :: Agent -> Bool
        isHome a = distance (position a) (0,0) < 1.0

shouldUpdate :: Agent -> Bool
shouldUpdate a = case updatePredicate a of
                   Just f  -> f a
                   Nothing -> True

levyModel :: Double -> Double -> Double -> Double -> Double -> Double -> Int -> UpdateRule -> IO Model
levyModel arenaSize commRange agentSpeed mu minStep maxStep numAgents update = do
  g <- newStdGen
  return $ newModel arenaSize commRange agentSpeed numAgents (initSquare s) update walk g
  where walk :: Agent -> Agent
        walk a =
          if shouldUpdate a
          then let (d, gen)  = randomR (0,1) (rng a)
                   (newHeading, gen') = randomR (0, 2*pi) gen
                   (currentX, currentY) = position a
                   stepLength = powerLaw minStep maxStep mu d
                   x = currentX + (stepLength * cos newHeading)
                   y = currentY + (stepLength * sin newHeading)
                   target = reflect (x, y) newHeading
               in a { rng = gen
                    , updatePredicate = Just (isAt target)
                    , heading = newHeading
                    }
          else a

        isAt t a = distance (position a) t < 1.0

        s = ceiling . sqrt $ fromIntegral numAgents

        reflect (x,y) heading
          | (x < maxCoord) && (x > minCoord) && (y < maxCoord) && (y > minCoord) = (x, y)
          | otherwise = let (p, h) = bounce x y heading in reflect p h

        minCoord = -(arenaSize / 2)
        maxCoord = (arenaSize / 2)

        xMin = minCoord
        yMin = minCoord
        xMax = maxCoord
        yMax = maxCoord

        bounce :: Double -> Double -> Double -> (Point, Double)
        bounce heading x y
          | x > xMax && y > yMax =
            let heading' = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMax), y - 2 * (y - yMax)), heading'')
          | x < xMin && y < yMin =
            let heading'  = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMin), y - 2 * (y - yMin)), heading'')
          | x > xMax && y < yMin =
            let heading'  = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMax), y - 2 * (y - yMin)), heading'')
          | x < xMin && y > yMax =
            let heading'  = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMin), y - 2 * (y - yMax)), heading'')
          | x < xMin             = ((x - 2 * (x - xMin), y), reflectY heading)
          | x > xMax             = ((x - 2 * (x - xMax), y), reflectY heading)
          | y < yMin             = ((x, y - 2 * (y - yMin)), reflectX heading)
          | y > yMax             = ((x, y - 2 * (y - yMax)), reflectX heading)
          | otherwise = ((x, y), heading)

-- reflect a unit vector across the x-axis returning the direction of
-- the resulting vector
reflectX :: Double -> Double
reflectX direction =
  let (x, y) = (cos direction, sin direction)
      theta  = atan ((-y) / x)
  in if x < 0 then theta + pi else if (-y) < 0 then theta + (2*pi) else theta

-- reflect a unit vector across the y-axis returning the direction of
-- the resulting vector
reflectY :: Double -> Double
reflectY direction =
  let (x, y) = (cos direction, sin direction)
      theta  = atan (y / (-x))
  in if (-x) < 0 then theta + pi else if y < 0 then theta + (2*pi) else theta

stepModel :: Double -> Model -> Model
stepModel stepSize m = m { time = time'
                         , nextUpdate = nextUpdate'
                         , agents = agents' }
  where agents' = if time' >= (nextUpdate m)
                  then mapAgents ((move m) . moveAgent) m
                  else mapAgents moveAgent m

        nextUpdate' = if time' >= (nextUpdate m)
                      then fromIntegral $ ceiling time'
                      else nextUpdate m

        time' = time m + stepSize

        xMax = size m
        yMax = size m
        xMin = -(size m)
        yMin = -(size m)

        moveAgent :: Agent -> Agent
        moveAgent agent =
          let (x,y) = position agent
              y'    = y + (stepSize * (speed agent)) * (sin $ heading agent)
              x'    = x + (stepSize * (speed agent)) * (cos $ heading agent)
              (position', heading') = bounce (heading agent) x' y'
          in
            agent { position = position', heading = heading' }

        bounce :: Double -> Double -> Double -> (Point, Double)
        bounce heading x y
          | x > xMax && y > yMax =
            let heading' = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMax), y - 2 * (y - yMax)), heading'')
          | x < xMin && y < yMin =
            let heading'  = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMin), y - 2 * (y - yMin)), heading'')
          | x > xMax && y < yMin =
            let heading'  = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMax), y - 2 * (y - yMin)), heading'')
          | x < xMin && y > yMax =
            let heading'  = reflectX heading
                heading'' = reflectY heading'
            in
              ((x - 2 * (x - xMin), y - 2 * (y - yMax)), heading'')
          | x < xMin             = ((x - 2 * (x - xMin), y), reflectY heading)
          | x > xMax             = ((x - 2 * (x - xMax), y), reflectY heading)
          | y < yMin             = ((x, y - 2 * (y - yMin)), reflectX heading)
          | y > yMax             = ((x, y - 2 * (y - yMax)), reflectX heading)
          | otherwise            = ((x, y), heading)

runModel :: Double -> Int -> Model -> [Model]
runModel stepSize steps = take steps . iterate (stepModel stepSize)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

getInteractions :: Model -> [(Int,Int)]
getInteractions m =
  [ (agentID a1, agentID a2) | a1 <- agents m, a2 <- agents m, distance (position a1) (position a2) < d, a1 /= a2]
  where d = range m

mapAgents :: (Agent -> a) -> Model -> [a]
mapAgents f m = map f (agents m)
