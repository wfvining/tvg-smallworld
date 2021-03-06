-- An ugly and bad config parser.
module Config
  ( TVGConfig(..)
  , MovementStrategy(..)
  , Rule(..)
  , Init(..)
  , loadConfig
  ) where

import Control.Applicative
import Text.ParserCombinators.ReadP

data Rule = Identity | SimpleMajority deriving Show
data Init = Square | Origin | Uniform deriving Show

data MovementStrategy = Ballistic
                      | CRW      { sigma :: Double }
                      | Levy     { alpha :: Double
                                 , maxStep :: Double }
                      | Homing   { sigma :: Double
                                 , pHome :: Double }
                      | Teleport { pJump :: Double }

data TVGConfig = TVGConfig { seed :: Int
                           , nAgents :: Int
                           , agentSpeed :: Double
                           , worldSize :: Double
                           , numRepetitions :: Int
                           , rate :: Double
                           , maxSteps :: Int
                           , movementStrategy :: MovementStrategy
                           , initialDensity :: Double
                           , communicationRange :: Double
                           , updateRule :: Rule
                           , initialize :: Init
                           } deriving Show

instance Show MovementStrategy where
    show (Levy a m) = "levy-" ++ (show a) ++ "-" ++ show m
    show (Homing s p) = "home-" ++ (show s) ++ "-" ++ show p
    show (Teleport p) = "teleport-" ++ show p
    show (CRW s) = "crw-" ++ show s
    show Ballistic = "ballistic"


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
  comm  <- parameter "communication-range"
  size  <- parameter "size"
  reps  <- integerParameter "repetitions"
  rate  <- parameter "rate"
  maxs  <- integerParameter "max-ticks"
  r     <- rule
  i     <- initializer
  return $ TVGConfig { seed = seed
                     , nAgents = n
                     , agentSpeed = speed
                     , communicationRange = comm
                     , worldSize = size
                     , numRepetitions = reps
                     , rate = rate
                     , maxSteps = maxs
                     , initialDensity = d
                     , movementStrategy = strat
                     , updateRule = r
                     , initialize = i}

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

rule :: ReadP Rule
rule = do
  param "ca-rule"
  (string "simple-majority\n" >> return SimpleMajority) <|> (string "identity\n" >> return Identity)

initializer :: ReadP Init
initializer = do
  param "init"
  square <|> origin <|> uniform

square = string "square\n" >> return Square
origin = string "origin\n" >> return Origin
uniform = string "uniform\n" >> return Uniform

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
    

