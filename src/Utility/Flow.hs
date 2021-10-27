module Utility.Flow where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Servant (Handler, ServerError, runHandler)
import Types.Environment (Environment (Environment))

type Flow = ReaderT Environment Handler

runFlow :: Flow a -> Environment -> Handler a
runFlow = runReaderT

runFlowIO :: Environment -> Flow a -> IO ()
runFlowIO env f = do
  runHandler (runEnv env f)
  pure ()

runEnv :: Environment -> Flow a -> Handler a
runEnv env f = runReaderT f env

getEnvironment :: Flow Environment
getEnvironment = ask
