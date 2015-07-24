{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.Consul.DockerClient where

import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (pack, Text)
import Text.Read (readMaybe)
import Data.Convertible (convert)
import Network.Socket (PortNumber(..))
import Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import Network.Consul (initializeConsulClient)
import Network.Consul.Types (ConsulClient(..), RegisterService(..))

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
  initializeConsulClient host (PortNum port) Nothing

type Service = Text
                         
mkRegisterService :: Service -> PortNumber -> [Text] -> RegisterService
mkRegisterService service (PortNum port) tags = RegisterService Nothing service tags (Just $ convert port) Nothing

mkManager :: IO Manager
mkManager = newManager defaultManagerSettings
