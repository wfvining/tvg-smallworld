module Model
  ( Agent(..)
  , Model(..)
  , MovementStrategy(..)
  , Initializer(..)
  , Point(..)
  , headingStrategy
  , positionStrategy2
  , positionStrategy3
  , identityStrategy
  , newModel
  , mapAgents
  , getInteractions
  , stepModel
  , runModel
  ) where

type Point = (Double, Double)

-- a movement strategy returns a new heading and a new movement strategy
data MovementStrategy = Heading (Agent -> (Double, MovementStrategy))
                      | Position (Agent -> (Point, MovementStrategy))
                      | Identity

type Initializer = (Int -> (Point, Double))

data Agent = Agent { agentID  :: Int
                   , position :: Point
                   , speed    :: Double
                   , heading  :: Double
                   , update   :: MovementStrategy
                   }

instance Eq Agent where
  a1 == a2 = agentID a1 == agentID a2

data Model = Model { agents :: [Agent] -- list of agents
                   , size   :: Double  -- bounds of the arena
                   , range  :: Double  -- communication range of the agents
                   , numAgents :: Int  -- number of agents
                   }

headingStrategy :: [Double] -> (Double -> Double -> Double) -> MovementStrategy
headingStrategy (s:state) f = Heading (\a -> (f (heading a) s, headingStrategy state f))

positionStrategy  :: [Double] -> (Point -> Double -> Point) -> MovementStrategy
positionStrategy  (s:state) f = Position (\a -> (f (position a) s, positionStrategy state f))

positionStrategy2 :: [Double] -> (Point -> Double -> Double -> Point) -> MovementStrategy
positionStrategy2 (s:s':state) f = Position (\a -> (f (position a) s s', positionStrategy2 state f))

positionStrategy3 :: [Double] -> (Point -> Double -> Double -> Double -> Point) -> MovementStrategy
positionStrategy3 (s:s':s'':state) f = Position (\a -> (f (position a) s s' s'', positionStrategy3 state f))

identityStrategy :: MovementStrategy
identityStrategy = Identity

newModel :: Double
         -> Double
         -> Double
         -> Initializer
         -> [MovementStrategy]
         -> Model
newModel size range speed init strategies =
  Model { agents = makeAgents speed init strategies
        , size   = size
        , range  = range
        , numAgents = length strategies
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
                                                , update   = strat }
                                      | (i, strat) <- zip [0..] movement ]

updateAgent :: Agent -> Agent
updateAgent agent = case update agent of
  Heading f  -> let (heading', update') = f agent in agent { heading = heading', update = update' }
  Position f -> let (position', update') = f agent in agent { position = position', update = update' }
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
stepModel stepSize m = m { agents = map (updateAgent . moveAgent) $ agents m }
  where xMax = size m
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

getInteractions :: Model -> [(Int,Int)]
getInteractions m =
  [ (agentID a1, agentID a2) | a1 <- agents m, a2 <- agents m, distance (position a1) (position a2) < d, a1 /= a2]
  where d = range m
        distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

mapAgents :: (Agent -> a) -> Model -> [a]
mapAgents f m = map f (agents m)
