module App.App where

import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)
import Server.API (appWheather)
import Types.Environment (Environment (port))
import Updater.Updater (update)
import Utility.Environment (makeEnvironment)
import Utility.Flow (Flow, runFlow, runFlowIO)

app :: IO ()
app = do
  eEnv <- makeEnvironment
  case eEnv of
    Left err -> print err
    Right env -> do
      res <- forkIO $ runFlowIO env update
      let appPort = port env
      run appPort (appWheather env)
