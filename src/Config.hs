-- An ugly and bad config parser.
module Config
  ( TVGConfig(..)
  , MovementStrategy(..)
  , loadConfig
  ) where

import Control.Applicative
import Text.ParserCombinators.ReadP

data MovementStrategy = Ballistic
                      | CRW      { sigma :: Double }
                      | Levy     { alpha :: Double
                                 , maxStep :: Double }
                      | Homing   { sigma :: Double
                                 , pHome :: Double }
                      | Teleport { pJump :: Double }
                      deriving Show

data TVGConfig = TVGConfig { seed :: Int
                           , numAgents :: Int
                           , agentSpeed :: Double
                           , worldSize :: Double
                           , numRepetitions :: Int
                           , rate :: Double
                           , maxSteps :: Int
                           , movementStrategy :: MovementStrategy
                           , initialDensity :: Double
                           } deriving Show

loadConfig :: FilePath -> IO TVGConfig
loadConfig configFile = do
  conf <- readFile configFile
  return (fst . head $ readP_to_S configuration conf)

configuration :: ReadP TVGConfig
configuration = do
  strat <- strategy
  seed  <- integerParameter "seed"
  n     <- integerParameter "num-agents"
  d     <- parameter "initial-density"
  speed <- parameter "agent-speed"
  size  <- parameter "size"
  reps  <- integerParameter "repetitions"
  rate  <- parameter "rate"
  maxs  <- integerParameter "max-ticks"
  return $ TVGConfig { seed = seed
                     , numAgents = n
                     , agentSpeed = speed
                     , worldSize = size
                     , numRepetitions = reps
                     , rate = rate
                     , maxSteps = maxs
                     , initialDensity = d
                     , movementStrategy = strat }

strategyName :: ReadP String
strategyName = do
  name <- string "ballistic"
          <|> string "crw"
          <|> string "levy"
          <|> string "homing"
          <|> string "teleport"
  char '\n'
  return name

digit :: ReadP Char
digit = satisfy (\ch -> ch >= '0' && ch <= '9')

positiveFloat :: ReadP Double
positiveFloat = do
  integral   <- read <$> many1 digit
  fractional <- option 0 fractionalParser
  return $ (integral + fractional)
    where fractionalParser = do
            char '.'
            rest <- many1 digit
            return $ read ("0."++rest)

integerParameter :: String -> ReadP Int
integerParameter name = do
  param name
  i <- read <$> many1 digit
  char '\n'
  return i

param :: String -> ReadP ()
param name = do
  string name
  skipSpaces
  char '='
  skipSpaces

parameter :: String -> ReadP Double
parameter name = do
  param name
  p <- positiveFloat
  char '\n'
  return p

strategyConfig "ballistic" = return Ballistic
strategyConfig "crw" = do
  s <- parameter "sigma"
  return $ CRW { sigma = s }
strategyConfig "levy" = 
  alphaThenStep <++ stepThenAlpha
    where alphaThenStep = do
            a <- parameter "alpha"
            l <- parameter "max-step"
            return $ Levy { alpha = a, maxStep = l }
          stepThenAlpha = do
            l <- parameter "max-step"
            a <- parameter "alpha"
            return $ Levy { alpha = a, maxStep = l }
strategyConfig "teleport" = do
  p <- parameter "pjump"
  return $ Teleport { pJump = p }
strategyConfig "homing" =
  homeThenSigma <++ sigmaThenHome
    where homeThenSigma = do
            ph <- parameter "phome"
            s  <- parameter "sigma"
            return $ Homing { pHome = ph, sigma = s }
          sigmaThenHome = do
            s  <- parameter "sigma"
            ph <- parameter "phome"
            return $ Homing { pHome = ph, sigma = s }

strategy :: ReadP MovementStrategy
strategy = do
  strat <- strategyName
  strategyConfig strat
    
