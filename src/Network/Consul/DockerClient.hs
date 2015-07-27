{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.Consul.DockerClient (registerService, deregisterService, mkConsulClient, mkRegisterService, mkDatacenter) where

import System.Environment (lookupEnv)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack)
import Text.Read (readMaybe)
import Data.Convertible (convert)
import Network.Socket (PortNumber(..))
import Network.Consul.Types (ConsulClient(..), RegisterService(..), Datacenter(..))
import qualified Network.Consul.Internal as I
import qualified Network.Consul as C

registerService :: ConsulClient -> RegisterService -> Maybe Datacenter -> IO ()
registerService c s d = do
  _ <- C.registerService c s d
  return ()

deregisterService :: ConsulClient -> RegisterService -> IO ()
deregisterService cc rs = do
  _ <- I.deregisterService (ccManager cc) (ccHostname cc) (ccPort cc) (rsName rs)
  return ()
         
mkConsulClient :: MonadIO m => m ConsulClient
mkConsulClient = do
  ch <- liftIO $ lookupEnv "CONSUL_HOST"
  let host = case ch of
               Just h -> pack h
               Nothing -> pack "localhost"
  cp <- liftIO $ lookupEnv "CONSUL_PORT"
  let port = case cp of
               Just p -> case (readMaybe p) of
                           Just p' -> fromInteger p'
                           Nothing -> 8500
               Nothing -> 8500
  C.initializeConsulClient host (PortNum port) Nothing

type Service = Text
type Tag = Text    

mkRegisterService :: Service -> PortNumber -> [Tag] -> RegisterService
mkRegisterService service (PortNum port) tags = RegisterService Nothing service tags (Just $ convert port) Nothing

mkDatacenter :: Text -> Datacenter
mkDatacenter dc = Datacenter dc
