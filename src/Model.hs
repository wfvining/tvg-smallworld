module Model
  ( Agent(..)
  , Model(..)
  , MovementStrategy(..)
  , Initializer(..)
  , Point(..)
  , AgentColor(..)
  , headingStrategy
  , positionStrategy2
  , positionStrategy3
  , velocityStrategy
  , identityStrategy
  , newModel
  , mapAgents
  , getInteractions
  , stepModel
  , runModel
  , levyModel
  , crwModel
  , brownianModel
  , teleportationModel
  , distance
  ) where

import Control.Monad
import System.Random
import Data.Random.Normal

type Point = (Double, Double)

-- a movement strategy returns a new heading and a new movement strategy
data MovementStrategy = Heading (Agent -> (Double, MovementStrategy))
                      | Position (Agent -> (Point, MovementStrategy))
                      | Velocity (Agent -> (Double, MovementStrategy))
                      | Comp MovementStrategy MovementStrategy
                      | Identity

type Initializer = (Int -> (Point, Double))

data AgentColor = Red | Green | Blue | White

data Agent = Agent { agentID  :: Int
                   , position :: Point
                   , speed    :: Double
                   , heading  :: Double
                   , update   :: MovementStrategy
                   , state    :: AgentColor
                   }

instance Eq Agent where
  a1 == a2 = agentID a1 == agentID a2

data Model = Model { agents :: [Agent] -- list of agents
                   , size   :: Double  -- bounds of the arena
                   , range  :: Double  -- communication range of the agents
                   , numAgents :: Int  -- number of agents
                   , time   :: Double
                   , nextUpdate :: Double
                   }

headingStrategy :: [Double] -> (Double -> Double -> Double) -> MovementStrategy
headingStrategy (s:state) f = Heading (\a -> (f (heading a) s, headingStrategy state f))

positionStrategy  :: [Double] -> (Point -> Double -> Point) -> MovementStrategy
positionStrategy  (s:state) f = Position (\a -> (f (position a) s, positionStrategy state f))

positionStrategy2 :: [Double] -> (Point -> Double -> Double -> Point) -> MovementStrategy
positionStrategy2 (s:s':state) f = Position (\a -> (f (position a) s s', positionStrategy2 state f))

positionStrategy3 :: [Double] -> (Point -> Double -> Double -> Double -> Point) -> MovementStrategy
positionStrategy3 (s:s':s'':state) f = Position (\a -> (f (position a) s s' s'', positionStrategy3 state f))

velocityStrategy :: [Double] -> (Double -> Double -> Double) -> MovementStrategy
velocityStrategy (s:state) f = Velocity (\a -> (f (speed a) s, velocityStrategy state f))

identityStrategy :: MovementStrategy
identityStrategy = Identity

newModel :: Double
         -> Double
         -> Double
         -> Initializer
         -> [MovementStrategy]
         -> Model
newModel size range speed init strategies =
  Model { agents     = makeAgents speed init strategies
        , size       = size/2
        , range      = range
        , numAgents  = length strategies
        , time       = 0
        , nextUpdate = 0
        }

makeAgents :: Double
           -> Initializer
           -> [MovementStrategy]
           -> [Agent]
makeAgents speed init movement = [ let (position, heading) = init i in
                                          Agent { agentID  = i
                                                , position = position
                                                , speed    = speed
                                                , heading  = heading
                                                , update   = strat
                                                , state    = White }
                                      | (i, strat) <- zip [0..] movement ]

teleport :: Double -> Double -> Double -> (Point -> Double -> Double -> Double -> Point)
teleport p w h = (\c p' x y -> if p' < p then (w*(x - 0.5), h*(y - 0.5)) else c)

brownian :: Double -> Double -> Double
brownian _ = id

crw :: Double -> Double -> Double
crw h t = let x = h + t in
  if x >= 0 && x <= 2*pi then x else x - 2*pi*(fromIntegral $ floor (x / (2*pi)))

levyWalk :: [Double] -> [Double] -> MovementStrategy
levyWalk xs ys = Comp (velocityStrategy xs (\x y -> y)) (headingStrategy ys brownian)

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

teleportationModel :: Double -> Double -> Double -> Double -> Int -> IO Model
teleportationModel arenaSize commRange agentSpeed p numAgents = do
  gens <- replicateM numAgents newStdGen
  let rs             = map (randomRs (1,0::Double)) gens
      teleportations = (zipWith positionStrategy3 rs (repeat (teleport p arenaSize arenaSize)))
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) teleportations

brownianModel :: Double -> Double -> Double -> Int -> IO Model
brownianModel arenaSize commRange agentSpeed numAgents = do
  gens <- replicateM numAgents newStdGen
  let rs = map (randomRs (0, 2*pi)) gens
      turns = zipWith headingStrategy rs (repeat brownian)
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) turns

crwModel :: Double -> Double -> Double -> Double -> Int -> IO Model
crwModel arenaSize commRange agentSpeed sigma numAgents = do
   rs <- replicateM numAgents (normalsIO' (0.0, sigma))
   let turns = zipWith headingStrategy rs (repeat crw)
   return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) turns

levyModel :: Double -> Double -> Double -> Double -> Double -> Double -> Int -> IO Model
levyModel arenaSize commRange agentSpeed mu minStep maxStep numAgents = do
  gens  <- replicateM numAgents newStdGen
  gens' <- replicateM numAgents newStdGen
  let rs = map (randomRs (0, 2*pi)) gens
      rs' = map (map (powerLaw minStep maxStep mu)) $ map (randomRs (0,1)) gens'
      walks = zipWith levyWalk rs' rs
  return $ newModel arenaSize commRange agentSpeed (initSquare (ceiling . sqrt $ fromIntegral numAgents)) walks

updateAgent :: Agent -> Agent
updateAgent agent = case update agent of
  Comp s1 s2 -> let a = updateAgent $ agent { update = s1 }
                    a' = updateAgent $ a { update = s2 }
                in a' { update = Comp (update a) (update a') }
  Heading f  -> let (heading', update') = f agent in agent { heading = heading', update = update' }
  Position f -> let (position', update') = f agent in agent { position = position', update = update' }
  Velocity f -> let (speed', update') = f agent in agent { speed = speed', update = update' }
  Identity   -> agent

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
                  then map (updateAgent . moveAgent) $ agents m
                  else map moveAgent $ agents m

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
