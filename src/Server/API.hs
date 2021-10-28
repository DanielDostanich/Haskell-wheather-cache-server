{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.API (appWheather) where

import           Servant                       (Application,
                                                HasServer (ServerT), Proxy (..),
                                                hoistServer, serve,
                                                type (:<|>) (..))
import           Server.Endpoints.GetByCity    (GetByCity, getByCity)
import           Server.Endpoints.GetByCityId  (GetByCityId, getByCityId)
import           Server.Endpoints.GetByCoords  (GetByCoords, getByCoords)
import           Server.Endpoints.GetByZipCode (GetByZipCode, getByZipCode)
import           Types.Environment             (Environment)
import           Utility.Flow                  (Flow, runEnv)

type API = GetByCity :<|> GetByCityId :<|> GetByCoords :<|> GetByZipCode

type FlowServer api = ServerT api Flow

apiWheather :: Proxy API
apiWheather = Proxy

serverWheather :: FlowServer API
serverWheather = getByCity :<|> getByCityId :<|> getByCoords :<|> getByZipCode

appWheather :: Environment -> Application
appWheather env = serve apiWheather (hoistServer apiWheather (runEnv env) serverWheather)
