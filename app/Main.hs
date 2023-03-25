{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Circuit
  ( Circuit,
    decodeCircuit,
    encodeCircuit,
    fromList,
    toList, noop,
  )
import Control.Applicative
  ( Applicative (pure, (*>), (<*), (<*>)),
    (<$>),
  )
import Control.Monad ((=<<))
import Control.Monad qualified as Monad
import Control.Monad.ST (stToIO)
import Data.Array.BitArray as BitArray
import Data.Array.BitArray.ByteString (fromByteString, toByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (Char, ord)
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (($>))
import Data.List qualified as List
import Data.Monoid ((<>))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Tuple (fst, snd)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Stats (RTSStats (nonmoving_gc_cpu_ns))
import Moo.GeneticAlgorithm.Binary
  ( Cond (Generations),
    Genome,
    IOHook (DoEvery, TimeLimit),
    Objective,
    ObjectiveFunction (..),
    ProblemType (Minimizing),
    Rand,
    SelectionOp,
    StepGA,
    constFrequencyMutate,
    loopIO,
    nextSteadyState,
    onePointCrossover,
    rankScale,
    rouletteSelect,
    runIO,
    withPopulationTransform, Population,
  )
import Moo.GeneticAlgorithm.Run
  ( Cond (Generations),
    IOHook (DoEvery, TimeLimit),
    loopIO,
    nextSteadyState,
    runIO,
  )
import Moo.GeneticAlgorithm.Statistics (quantiles)
import Moo.GeneticAlgorithm.Types
  ( Cond (Generations),
    Genome,
    ProblemType (Minimizing),
    SelectionOp,
    StepGA,
  )
import Options (Options (..), getOptions)
import Relude hiding (fromList)
import Serialization (Serialize (..), deserialize, TextParser)
import Test.QuickCheck (Gen, arbitrary, chooseInt, generate, vectorOf)
import Utils (scoreLines)
import Moo.GeneticAlgorithm.Binary (Cond(..))

main :: IO ()
main = do
  Text.putStrLn "Welcome to the Circuit Optimizer!"
  print =<< getArgs
  (population, timeLimit, output, plainText) <- getInput
  result <- body (toChunks 100 plainText) population timeLimit
  handleResult output result

toChunks :: Int -> ByteString -> Vector ByteString
toChunks n = Vector.unfoldr step
  where
    step bs
      | BS.null bs = Nothing
      | otherwise = Just (BS.splitAt n bs)

getInput :: IO ([Circuit], Int, FilePath, ByteString)
getInput = do
  print =<< getOptions
  Options {..} <- getOptions
  plainText <- BS.readFile plainText
  case (startPopulation == 0, input == "") of
    (True, True) -> die "No input file or population size given. Exiting."
    (False, False) -> die "Both input file and population size given. Exiting."
    (True, False) -> do
      population <- readPopulationFile input
      pure (population, timeLimit, output, plainText)
    (False, True) -> do
      population <- generatePopulation startPopulation
      pure (population, timeLimit, output, plainText)

readPopulationFile :: FilePath -> IO [Circuit]
readPopulationFile filePath = do
  inputText <- Text.readFile filePath
  deserialize "failed to read population file:" filePath inputText

generatePopulation :: Int -> IO [Circuit]
generatePopulation n = do
  randomlyMade <- Monad.replicateM (n - 1) (generate $ Vector.replicateM 100 arbitrary)
  pure $ noops : randomlyMade
  where
    noops :: Circuit
    noops = Vector.replicate 100 noop

body :: Vector ByteString -> [Circuit] -> Int -> IO [(Circuit, Double)]
body plainText population' timeLimit = do
  proctor <- makeProctor 100 plainText
  putStr "Starting fitnesses: "
  printStats (-1) $ zip population (proctor population)
  let (stepGA :: StepGA Rand Bool) = nextSteadyState (List.length population `div` 2) Minimizing proctor selectionOp (onePointCrossover 0.5) (constFrequencyMutate 1)
  let ga = loopIO [TimeLimit (fromIntegral timeLimit), DoEvery 5 printStats] cond stepGA
  List.map (first decodeCircuit) <$> runIO (pure population) ga
  where
    selectionOp :: SelectionOp a
    selectionOp = withPopulationTransform (rankScale Minimizing) (rouletteSelect 2)
    cond :: Cond a
    cond = GensNoChange 5 (quantiles [0.5, 0.0]) Nothing `And` IfObjective medianEqualsMin
    population = encodeCircuit <$> population'

-- if the median and minimum are the same, we're done
medianEqualsMin :: [Objective] -> Bool
medianEqualsMin scores =
  case quantiles [0.5, 0] scores of
    [median, min] -> median == min
    _ -> False

-- choose n lines from the input file and score the circuit against them
makeProctor :: Int -> Vector ByteString -> IO ([Genome Bool] -> [Objective])
makeProctor numLines v = do
  indexes <- generate $ Vector.replicateM numLines (chooseInt (0, Vector.length v - 1))
  let lines = Vector.map (v Vector.!) indexes
  pure (scoreLines lines . decodeCircuit <$>)

showT :: Show a => a -> Text.Text
showT = Text.pack . show

printStats :: Int -> [(Genome Bool, Double)] -> IO ()
printStats generationNumber population = do
  let fitnessQuantiles = quantiles (snd <$> population) [1.0, 0.9, 0.75, 0.5, 0.25, 0.1, 0.0]
  Text.putStrLn $ "Generation: " <> showT generationNumber <> " Quantiles: " <> showT fitnessQuantiles

handleResult :: FilePath -> [(Circuit, Double)] -> IO ()
handleResult "" result = do
  putStrLn "Ending fitnesses: "
  print (sort (snd <$> result))
handleResult output result = do
  handleResult "" result
  let bs = serialize $ List.map fst result
  Text.writeFile output bs
  putStrLn "Wrote result to file."
