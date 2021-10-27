module Updater.Updater where

import Cache.Redis.Set (tempsAdd)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Types.Environment
  ( Environment (autoUpdLocations, sleepTime),
  )
import Types.Wheather (RequestType)
import Utility.Flow (Flow, getEnvironment)
import Wheather.Reciever (receiveWheather)

update :: Flow ()
update = do
  liftIO $ print "Update started. Receiving errors will be displayed"
  env <- getEnvironment
  let locations = autoUpdLocations env
  let slTime = sleepTime env
  mapM_ handler locations
  liftIO $ print "Update finished"
  liftIO . threadDelay $ slTime * 1000000
  update
  where
    handler :: RequestType -> Flow ()
    handler request = do
      eWheather <- receiveWheather request
      either (liftIO . print) tempsAdd eWheather