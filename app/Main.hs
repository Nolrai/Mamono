{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Circuit
import Control.Monad qualified as Monad
import Control.Monad.ST (stToIO)
import Data.Array.BitArray (fromByteString, toByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moo.GeneticAlgorithm.Binary
import Options (Options (..), getOptions)
import Text.Megaparsec hiding (getInput)
import Utils (scoreCircuit)
import Data.ByteString.Base64 qualified as Base64

main :: IO ()
main = do
  (population, timeLimit, output) <- getInput
  result <- body plainText population timeLimit
  handleResult output result

getInput = do
  Options {..} <- getOptions
  plainText <- split (ord '\n') BS.readFile plainTextFile
  case (startPopulation == 0, input == "") of
    (True, True) -> die "No input file or population size given. Exiting."
    (False, False) -> die "Both input file and population size given. Exiting."
    (True, False) -> do
      population <- parseInput input
      return (population, timeLimit, output, plainText)
    (False, True) -> do
      population <- generatePopulation startPopulation
      return (population, timeLimit, output, plainText)

body :: Vector ByteString -> [Circuit] -> Int -> IO [(Circuit, Double)]
body plainText population timeLimit = runIO (pure population) ga
  where
    ga = loopIO [Stop timeLimit, DoEvery 1 printBest] cond stepGA
    stepGA :: StepGA Rand a
    stepGA = nextSteadyState (List.length population `div` 2) Minimizing (scoreCircuit plainText) selectOp (crossover 0.5) (mutate 0.1)
    selectOp = withPopulationTransform (rankScale Minimizing) . rouletteSelect
    cond :: Cond a
    cond = Generations 10000

handleResult :: FilePath -> [(Circuit, Integer)] -> IO ()
handleResult "" result = do
  putStrLn "Ending fitnesses: "
  print (map snd result)
handleResult output result _ = do
  putStrLn "Ending fitnesses: "
  print (map snd result)
  let bs = unparsePopulation result
  writeFile output bs
  putStrLn "Wrote result to file."

unparsePopulation :: [(Circuit, Integer)] -> BS.ByteString
unparsePopulation population =
  (pack . show . length) population
    <> singelton newline
    <> BS.intercalate (singelton newline) (map (unparseCircuit . fst) population)

unparseCircuit :: Circuit -> BS.ByteString
unparseCircuit circuit = header <> body
  where
  body = bs
  header = (pack . show . length) bs <> singleton (ord ' ')
            <> (pack . show . length) bitArray <> singleton (ord ' ')-- we need both of these because of padding issues and base64 encoding
  bs = Base64.encode . toBystring $ bitArray
  bitArray = fromList . encodeCircuit $ circuit

type BSParser = Parsec () BS.ByteString

parsePopulation :: BSParser [Circuit]
parsePopulation = do
  n <- single
  Monad.replicateM n parseCircuit

parseCircuit :: BSParser Circuit
parseCircuit = do
  byteLength <- decimal
  bitLength <- decimal
  base64 <- takeP "circuit" n
  let byteString = decodeLenient bs
  let bitArray = fromByteString (0, bitLength - 1) byteString
  let circuit = decodeCircuit . toList $ bitArray
  pure circuit
