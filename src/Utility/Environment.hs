{-# LANGUAGE RecordWildCards #-}

module Utility.Environment where

import Control.Exception (catch, SomeException)
import Database.Redis (ConnectError, Connection, checkedConnect, defaultConnectInfo)
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
import Utility.CLArguments (getWheaterAPIKey)
import Utility.Config (readConfig)

makeEnvironment :: IO (Either String Environment)
makeEnvironment = do
  eConfig <- readConfig
  case eConfig of
    Left err -> pure . Left $ err
    Right ConfigFields {..} -> do
      eConn <- connectToRedis
      case eConn of
        Left err -> pure . Left $ err
        Right conn -> do
          wheaterAPIKey <- getWheaterAPIKey
          pure . Right $ Environment {..}

connectToRedis :: IO (Either String Connection)
connectToRedis =
  catch
    ( do
        conn <- checkedConnect defaultConnectInfo
        pure . Right $ conn
    )
    (\e -> pure . Left $ "Redis connection error: " ++  show (e :: SomeException))