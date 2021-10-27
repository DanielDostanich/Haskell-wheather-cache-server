{-# LANGUAGE RecordWildCards #-}

module Utility.Environment where

import Database.Redis (checkedConnect, defaultConnectInfo)
import Types.Config as Config
  ( ConfigFields
      ( ConfigFields,
        autoUpdLocations,
        port,
        rangeError,
        sleepTime,
        timeError
      ),
  )
import Types.Environment (Environment (..))
import Utility.Config (readConfig)
import Utility.CLArguments (getWheaterAPIKey)

makeEnvironment :: IO (Either String Environment)
makeEnvironment = do
  eConfig <- readConfig
  case eConfig of
    Left err -> pure . Left $ err
    Right ConfigFields {..} -> do
      conn <- checkedConnect defaultConnectInfo
      wheaterAPIKey <- getWheaterAPIKey
      pure . Right $ Environment {..}
