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

-- Some simple 2D vector operations for vectors represened by tupples.
(<->) :: Num a => (a,a) -> (a,a) -> (a,a)
(<->) (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

(<+>) :: Num a => (a,a) -> (a,a) -> (a,a)
(<+>) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

(<.>) :: Num a => (a,a) -> (a,a) -> a
(<.>) (x1,y1) (x2,y2) = x1*x2 + y1*y2

(.*) :: Num a => a -> (a,a) -> (a,a)
(.*) s (x, y) = (s*x, s*y)

reflect :: Double -> Double -> (Double,Double) -> Double
reflect direction magnitude normal =
  let v  = (magnitude * cos direction, magnitude * sin direction)
      v' = v <-> ((2*(v <.> normal)) .* normal)
  in
    atan $ uncurry (/) v'

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
            let heading' = reflect heading speed (0,1)
                heading'' = reflect heading' speed (1,0)
            in
              ((x - 2 * (x - xMax), y - 2 * (y - yMax)), heading'')
          | x < xMin && y < yMin =
            let heading'  = reflect heading speed (0,-1)
                heading'' = reflect heading' speed (-1,0)
            in
              ((x - 2 * (x - xMin), y - 2 * (y - yMin)), heading'')
          | x > xMax && y < yMin =
            let heading'  = reflect heading speed (0,-1)
                heading'' = reflect heading' speed (1,0)
            in
              ((x - 2 * (x - xMax), y - 2 * (y - yMin)), heading'')
          | x < xMin && y > yMax =
            let heading'  = reflect heading speed (0,1)
                heading'' = reflect heading' speed (-1,0)
            in
              ((x - 2 * (x - xMin), y - 2 * (y - yMax)), heading'')
          | x < xMin             = ((x - 2 * (x - xMin), y), reflect heading speed (-1,0))
          | x > xMax             = ((x - 2 * (x - xMax), y), reflect heading speed (1,0))
          | y < yMin             = ((x, y - 2 * (y - yMin)), reflect heading speed (0,-1))
          | y > yMax             = ((x, y - 2 * (y - yMax)), reflect heading speed (0,1))
          | otherwise            = ((x, y), heading)
