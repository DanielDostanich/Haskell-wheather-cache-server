module Cache.Redis.Queries (setAdd, setGetRange, setRem, mapAdd, mapGet) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, decode, encode)
import Data.ByteString.Lazy.Char8 (ByteString, fromStrict, pack, toStrict, unpack)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Database.Redis
  ( Reply,
    hget,
    hset,
    runRedis,
    zadd,
    zrangebyscore,
    zrem,
  )
import qualified Types.Environment as Environment
import Types.Wheather (Coordinates)
import Utility.Flow (Flow, getEnvironment)

setAdd :: String -> Double -> Value -> Flow ()
setAdd key score member = do
  env <- getEnvironment
  let conn = Environment.conn env
  let keyBs = toStrict . pack $ key
  let value = [(score, toStrict $ encode member)]
  res <- liftIO $ runRedis conn (zadd keyBs value)
  either (printReturnDef ()) (\_ -> pure ()) res

setGetRange :: String -> Double -> Double -> Flow [Value]
setGetRange key min max = do
  env <- getEnvironment
  let conn = Environment.conn env
  let keyBs = toStrict . pack $ key
  res <- liftIO $ runRedis conn (zrangebyscore keyBs min max)
  either (printReturnDef []) (pure . mapMaybe (decode . fromStrict)) res

setRem :: String -> Value -> Flow ()
setRem key member = do
  env <- getEnvironment
  let conn = Environment.conn env
  let keyBs = toStrict . pack $ key
  let value = [toStrict $ encode member]
  res <- liftIO $ runRedis conn (zrem keyBs value)
  either (printReturnDef ()) (\_ -> pure ()) res

mapAdd :: String -> String -> ByteString -> Flow ()
mapAdd key field value = do
  env <- getEnvironment
  let conn = Environment.conn env
  let keyBs = toStrict . pack $ key
  let fieldBs = toStrict . pack $ field
  let valueBs = toStrict value
  res <- liftIO $ runRedis conn (hset keyBs fieldBs valueBs)
  either (printReturnDef ()) (\_ -> pure ()) res

mapGet :: String -> String -> Flow (Maybe ByteString)
mapGet key field = do
  env <- getEnvironment
  let conn = Environment.conn env
  let keyBs = toStrict . pack $ key
  let fieldBs = toStrict . pack $ field
  res <- liftIO $ runRedis conn (hget keyBs fieldBs)
  either (printReturnDef Nothing) (pure . fmap fromStrict) res

printReturnDef :: a -> Reply -> Flow a
printReturnDef def reply = do
  liftIO $ print reply
  pure def