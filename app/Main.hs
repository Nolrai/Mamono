module Main where

import System.Environment (getArgs)
import Text.Read ( readEither )

import Luhn (validate)

main :: IO ()
main = do
  [input] <- map readEither <$> getArgs
  case input of
    Left err -> error err
    Right int -> do
      let result = if validate int then "Valid" else "Invalid"
      putStrLn $ "running Luhn validation: " <> result
