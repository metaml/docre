{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Network.Consul.DockerClient (registerService
                                   , deregisterService
                                   , registerNode
                                   , deregisterNode
                                   , mkConsulClient)
where

import qualified Data.ByteString.Lazy as BL    
import qualified Data.ByteString.Internal as B
import qualified Network.Consul.Internal as I
import qualified Network.Consul.Types as C    
import qualified Network.Consul as C
import Prelude hiding (concat)  
import System.Environment (lookupEnv)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, concat, pack, unpack)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Data.Convertible (convert)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), (.=), object, encode)
import Network.Socket (PortNumber(..))
import Network.HTTP.Client (Request(..), RequestBody(..)
                           , parseUrl, withResponse, responseStatus)
import Network.HTTP.Types.Status (status200)
import Data.Consul
    
registerNode :: C.ConsulClient -> RegisterNode -> IO Bool
registerNode _client@C.ConsulClient{..} rr = do
  req <- mkRequest ccHostname ccPort "/v1/catalog/register" Nothing (Just $ BL.toStrict $ encode rr) False Nothing
  liftIO $ withResponse req ccManager $ \res -> do
    case responseStatus res of
      x | x == status200 -> return True
      _ -> return False

deregisterNode :: C.ConsulClient -> DeregisterNode -> IO Bool
deregisterNode _client@C.ConsulClient{..} dr = do
  req <- mkRequest ccHostname ccPort "/v1/catalog/deregister" Nothing (Just $ BL.toStrict $ encode dr) False Nothing
  liftIO $ withResponse req ccManager $ \res -> do
    case responseStatus res of
      x | x == status200 -> return True
      _ -> return False

registerService :: C.ConsulClient -> C.RegisterService -> Maybe C.Datacenter -> IO ()
registerService c s d = do
  _ <- C.registerService c s d
  return ()

deregisterService :: C.ConsulClient -> C.RegisterService -> IO ()
deregisterService _cc@C.ConsulClient{..} _rs@C.RegisterService{..} = do
  _ <- I.deregisterService ccManager ccHostname ccPort rsName
  return ()

mkConsulClient :: MonadIO m => m C.ConsulClient
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

type NodeName = Text
type Address = Text      
type Sid = Text   
type ServiceName = Text
type Tag = Text
type Port = Int
       
mkRequest :: MonadIO m => Text -> PortNumber -> Text -> Maybe Text -> Maybe B.ByteString -> Bool -> Maybe C.Datacenter -> m Request
mkRequest hostname (PortNum portNumber) endpoint query body wait dc = do
  let baseUrl = concat ["http://",hostname,":", pack $ show portNumber, endpoint, needQueryString
                       , maybe "" id query, prefixAnd, maybe "" (\(C.Datacenter x) -> concat ["dc=", x]) dc]
  initReq <- liftIO $ parseUrl $ unpack baseUrl
  case body of
    Just x -> return $ indef $ initReq {method = "PUT", requestBody = RequestBodyBS x, checkStatus = \_ _ _ -> Nothing}
    Nothing -> return $ indef $ initReq {checkStatus = \_ _ _ -> Nothing}
  where
    needQueryString = if isJust dc || isJust query then "?" else ""
    prefixAnd = if isJust query && isJust dc then "&" else ""
    indef req = if wait == True then req{responseTimeout = Nothing} else req
