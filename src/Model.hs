module Model
  ( Agent(..)
  , Model(..)
  , newModel
  ) where

import Prelude hiding (id)

type Point = (Double, Double)

-- a movement strategy returns a new heading and a new movement strategy
data MovementStrategy = Heading (Agent -> (Double, MovementStrategy))
                      | Position (Agent -> (Point, MovementStrategy))

type Initializer = (Int -> (Point, Double))

data Agent = Agent { id       :: Int
                   , position :: Point
                   , speed    :: Double
                   , heading  :: Double
                   , update   :: MovementStrategy
                   }

data Model = Model { agents :: [Agent] -- list of agents
                   , size   :: Double  -- bounds of the arena
                   , range  :: Double  -- communication range of the agents
                   }

headingStrategy :: [Double] -> (Double -> Double -> Double) -> MovementStrategy
headingStrategy (s:state) f = Heading (\a -> (f (heading a) s, headingStrategy state f))

positionStrategy :: [Double] -> (Point -> Double -> Double -> Point) -> MovementStrategy
positionStrategy (s:s':state) f = Position (\a -> (f (position a) s s', positionStrategy state f))

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
        }

makeAgents :: Double
           -> Initializer
           -> [MovementStrategy]
           -> [Agent]
makeAgents speed init movement = [ let (position, heading) = init i in
                                          Agent { id       = i
                                                , position = position
                                                , speed    = speed
                                                , heading  = heading
                                                , update   = strat }
                                      | (i, strat) <- zip [0..] movement ]

updateAgent :: Agent -> Agent
updateAgent agent = case update agent of
  Heading f  -> let (heading', update') = f agent in agent { heading = heading', update = update' }
  Position f -> let (position', update') = f agent in agent { position = position', update = update' }

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
      theta = atan (y / (-x))
  in if (-x) < 0 then theta + pi else if y < 0 then theta + (2*pi) else theta

stepModel :: Double -> Model -> Model
stepModel stepSize m = m { agents = map (updateAgent . moveAgent) $ agents m }
  where xMax = range m
        yMax = range m
        xMin = -(range m)
        yMin = -(range m)

        moveAgent :: Agent -> Agent
        moveAgent agent =
          let (x,y) = position agent
              y'    = y + (stepSize * (speed agent)) * (sin $ heading agent)
              x'    = x + (stepSize * (speed agent)) * (cos $ heading agent)
              (pos,heading') = bounce (stepSize * (speed agent)) (heading agent) x' y'
          in
            agent { position = pos, heading = heading' }

        bounce :: Double -> Double -> Double -> Double -> (Point, Double)
        bounce speed heading x y
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
          | y > yMax             = ((x, y - 2 * (y - yMax)), reflectY heading)
          | otherwise            = ((x, y), heading)
