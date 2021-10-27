module Utility.ConfigException where

import Control.Exception (Exception, throwIO)

newtype ConfigExc = ConfigExc {getExcText :: String}
  deriving (Show)

instance Exception ConfigExc

throwConfigExc :: String -> IO a
throwConfigExc msg = throwIO $ ConfigExc msg
