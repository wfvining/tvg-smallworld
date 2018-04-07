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

updateAgent :: Agent -> Agent
updateAgent agent = case update agent of
  Heading f  -> let (heading', update') = f agent in agent { heading = heading', update = update' }
  Position f -> let (position', update') = f agent in agent { position = position', update = update' }

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
