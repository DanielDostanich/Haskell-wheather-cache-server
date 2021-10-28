module Utility.Flow where

import           Control.Monad              (void)
import           Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import           Servant                    (Handler, runHandler)
import           Types.Environment          (Environment)

type Flow = ReaderT Environment Handler

runFlow :: Flow a -> Environment -> Handler a
runFlow = runReaderT

runFlowIO :: Environment -> Flow a -> IO ()
runFlowIO env f = void $ runHandler (runEnv env f)

runEnv :: Environment -> Flow a -> Handler a
runEnv env f = runReaderT f env

getEnvironment :: Flow Environment
getEnvironment = ask
