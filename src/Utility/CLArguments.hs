module Utility.CLArguments where

import           Data.Text                   (Text)
import           Options.Applicative         (Parser, ParserInfo, execParser,
                                              metavar)
import           Options.Applicative.Builder (fullDesc, header, help, info,
                                              long, progDesc, strOption)

parser :: Parser Text
parser = strOption (long "api" <> metavar "APIKey" <> help "API key for openweathermap")

opts :: ParserInfo Text
opts =
  info
    parser
    ( fullDesc
        <> progDesc "Cache-server for getting weather info. More information can be found in readme"
        <> header "task-test-getshop"
    )

getWheaterAPIKey :: IO Text
getWheaterAPIKey = do
  execParser opts
