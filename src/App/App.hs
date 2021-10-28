module App.App where

import           Control.Concurrent       (forkIO)
import           Control.Monad            (void)
import           Network.Wai.Handler.Warp (run)
import           Server.API               (appWheather)
import           Types.Environment        (Environment (port))
import           Updater.Updater          (update)
import           Utility.Environment      (makeEnvironment)
import           Utility.Flow             (runFlowIO)

app :: IO ()
app = do
  eEnv <- makeEnvironment
  case eEnv of
    Left err -> print err
    Right env -> do
      void $ forkIO $ runFlowIO env update -- Обработка ошибок?
      let appPort = port env
      run appPort (appWheather env)
