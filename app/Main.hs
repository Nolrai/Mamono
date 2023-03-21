{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Circuit
import Circuit (decodeCircuit, encodeCircuit)
import Control.Applicative
import Control.Arrow (first)
import Control.Monad qualified as Monad
import Control.Monad.ST (stToIO)
import Data.Array.BitArray as BitArray
import Data.Array.BitArray.ByteString (fromByteString, toByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Char
import Data.Function (($), (.))
import Data.Functor (($>))
import Data.List qualified as List
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Tuple (fst, snd)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Base (Bool (..), Double, IO, Int, Ord (..), otherwise, (&&), (||))
import GHC.Enum (Enum (..), fromEnum, toEnum)
import GHC.Num (Integer, Num (..))
import GHC.Real (Fractional (..), Integral (..), Real (..), fromIntegral, toRational)
import GHC.Show (Show (..))
import GHC.Word (Word8)
import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Statistics (quantiles)
import Moo.GeneticAlgorithm.Types
import Options (Options (..), getOptions)
import System.IO (FilePath, IO, print, putStrLn, readFile, writeFile)
import System.Exit (die)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer (decimal)
import Utils (scoreCircuit)

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
      population <- runParser parsePopulation input =<< BS.readFile input
      pure (population, timeLimit, output, plainText)
    (False, True) -> do
      population <- generatePopulation startPopulation
      pure (population, timeLimit, output, plainText)

generatePopulation :: Int -> IO [Circuit]
generatePopulation n = stToIO $ replicateM n (randomCircuit 100)

body :: Vector ByteString -> [Circuit] -> Int -> IO [(Circuit, Double)]
body plainText population timeLimit =
  first decodeCircuit <$> runIO (pure $ encodeCircuit <$> population) ga
  where
    ga = loopIO [TimeLimit (fromIntegral timeLimit), DoEvery 5 printStats] cond stepGA
    stepGA :: StepGA Rand Bool
    stepGA = nextSteadyState (List.length population `div` 2) Minimizing (scoreCircuit plainText) selectionOp (onePointCrossover 0.5) (constFrequencyMutate 1)
    selectionOp :: SelectionOp a
    selectionOp = withPopulationTransform (rankScale Minimizing) (rouletteSelect 2)
    cond :: Cond a
    cond = Generations 10000

showT :: Show a => a -> Text.Text
showT = Text.pack . show

printStats :: Int -> [(Genome Bool, Double)] -> IO ()
printStats generationNumber population = do
  let fitnessQuantiles = quantiles (snd <$> population) [1.0, 0.9, 0.75, 0.5, 0.25, 0.1, 0.0]
  Text.putStrLn $ "Generation: " <> showT generationNumber <> " Quantiles: " <> showT fitnessQuantiles

handleResult :: FilePath -> [(Circuit, Integer)] -> IO ()
handleResult "" result = do
  putStrLn "Ending fitnesses: "
  print (snd <$> result)
handleResult output result = do
  putStrLn "Ending fitnesses: "
  print (snd <$> result)
  let bs = unparsePopulation result
  BS.writeFile output bs
  putStrLn "Wrote result to file."

showToBS :: Show a => a -> ByteString
showToBS = Text.encodeUtf8 . Text.pack . show

unparsePopulation :: [(Circuit, Integer)] -> BS.ByteString
unparsePopulation population =
  showToBS (List.length population)
    <> charBS '\n'
    <> BS.intercalate (charBS '\n') (List.map (unparseCircuit . fst) population)

charBS :: Char -> BS.ByteString
charBS = BS.singleton . fromIntegral . fromEnum

unparseCircuit :: Circuit -> BS.ByteString
unparseCircuit circuit = header <> body
  where
    body = bs
    -- we need both of these lengths because of padding issues and base64 encoding
    header = showToBS (BS.length bs) <> charBS ' ' <> showToBS (bounds bitArray) <> charBS ' '
    bs = Base64.encode . toByteString $ bitArray
    bitArray = fromList . encodeCircuit $ circuit

type BSParser = Parsec () BS.ByteString

parsePopulation :: BSParser [Circuit]
parsePopulation = do
  n <- decimal
  Monad.replicateM n parseCircuit

char_ :: Char -> BSParser ()
char_ = (char . fromIntegral . fromEnum) $> ()

parseCircuit :: BSParser Circuit
parseCircuit = do
  byteLength <- decimal
  bitRange <- (,) <$> char_ '(' *> decimal <*> (char_ ',' *> decimal <* char_ ')')
  base64 <- takeP (Just "circuit") byteLength
  let byteString = Base64.decodeLenient base64
  let bitArray = fromByteString bitRange byteString
  let circuit = decodeCircuit . Circuit.toList $ bitArray
  pure circuit
