module Options where

import Options.Applicative

-- Command line parsing

outputParser =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output file"
        <> value ""
    )

inputParser =
  strOption
    ( long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Input file"
        <> value ""
    )

plainTextParser =
  strOption
    ( long "plain-text"
        <> short 'p'
        <> metavar "FILE"
        <> help "Plain text file to optimize against"
    )

timeLimitParser :: Parser Int
timeLimitParser =
  option
    auto
    ( long "time-limit"
        <> short 't'
        <> metavar "SECONDS"
        <> help "Time limit"
    )

startPopulationParser :: Parser Int
startPopulationParser =
  option
    auto
    ( long "start-population"
        <> short 's'
        <> metavar "NUMBER"
        <> help "Starting population size"
        <> value 0
    )

data Options = Options
  { output :: FilePath,
    input :: FilePath,
    plainText :: FilePath,
    timeLimit :: Int,
    startPopulation :: Int
  }
  deriving (Show)

optionParser :: Parser Options
optionParser = Options <$> outputParser <*> inputParser <*> plainTextParser <*> timeLimitParser <*> startPopulationParser

getOptions :: IO Options
getOptions = execParser opts
  where
    opts =
      info
        (optionParser <**> helper)
        ( fullDesc
            <> progDesc "Optimize a circuit"
            <> header "circuit-optimize - optimize a circuit"
        )
