{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStrLn, getLine, words, lines, concat, length, drop, dropWhile, null, lookup)
import Network.Socket hiding (recv)
-- import qualified GHC.IO.Exception as Ex
-- import Control.Exception (try, throwIO)
import qualified Data.HashMap.Strict as Map
import System.Directory (doesFileExist)    
import Control.Monad (forever)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson (decodeStrict)
import Data.Foldable (forM_)
import Data.ByteString (concat, ByteString)
import Data.ByteString.Char8 (putStrLn, pack, unpack)
import Data.Text (Text, append, dropWhile, replace, splitOn)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Read (decimal)
import Text.Regex (mkRegex, splitRegex)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer, Pipe)
import Data.Docker (Event(..))
import Data.Consul (RegisterNode(..), DeregisterNode(..), Service(..), Datacenter(..)
                   ,ConsulStartResponse(..), ConsulStartNetworkSettings(..))    
import Network.Consul.DockerClient (registerNode, deregisterNode, mkConsulClient)
import Control.Concurrent (threadDelay)    
import Control.Monad (when)
import Control.Monad.Trans ()
import Control.Monad.Trans.Either

main :: IO ()
main = do
  e <- runEitherT $ forever $ do
         sock <- lift $ doesFileExist "/var/run/docker.sock"
         lift $ print sock
         _ <- lift $ threadDelay 1000000
         when (sock == True) $ left ()
  case e of
    Left  _ -> dockerListener
    Right _ -> main
    
dockerListener :: IO ()
dockerListener = runEffect $ docker >-> json2event >-> event2id >-> id2container >-> container2consul >-> consul

type Status = ByteString
type Cid = ByteString
type Name = ByteString
type Ip = ByteString        
type Services = [ByteString]
    
-- | @todo: handle error cases
docker :: Producer ByteString IO ()
docker = forever $ do
           s <- lift $ unixSocket       
           _ <- ($) forever $ lift (event s) >>= yield 
           lift $ sClose s

json2event :: Pipe ByteString Event IO ()
json2event = forever $ do
               r <- await
               let j = last $ filter (\c-> c /= "") $ (splitRegex (mkRegex "[ \t\r\n]+") r') where r' = unpack r
               lift $ print j
               case (decodeStrict $ pack j) of
                 Nothing -> lift $ putStrLn "error in json2event:" >> print j
                 Just e -> yield e

event2id :: Pipe Event (Cid, Status) IO ()
event2id = forever $ do
             e <- await
             yield $ (encodeUtf8 $ eventId e, encodeUtf8 $ eventStatus e)

id2container :: Pipe (Cid, Status) (ByteString, Status) IO ()
id2container = forever $ do
                 s <- lift $ unixSocket
                 _ <- ($) forever $ do
                           (eId, status) <- await
                           lift $ sendAll s $ concat ["GET /containers/", eId, "/json HTTP/1.1", "\r\n\r\n"]
                           r <- lift $ recv s 4096 -- r: http response
                           yield $ (r, status)
                 lift $ sClose s

container2consul :: Pipe (ByteString, Status) (ByteString, Status) IO ()
container2consul = forever $ do
                     (r, status) <- await  -- r: http response
                     let json :: ByteString = pack $ last $ init $ filter (\c -> c /= "")
                                              (splitRegex (mkRegex "[ \t\r\n]+") (unpack r))
                     lift $ print json                          
                     yield (json, status)
                                 
consul :: Consumer (ByteString, Status) IO ()
consul = do
  consulClient <- mkConsulClient
  forever $ do
    (json, status) <- await
    r <- case status of
           "start" -> do
             -- @todo: bug--csr = Nothing but json looks legit                       
             let csr :: Maybe ConsulStartResponse = decodeStrict json
                 nodes = mkRegisterNodes csr
             lift $ print csr
             case nodes of
               Just ns -> forM_ ns (\n -> lift $ registerNode consulClient n) >> return True
               Nothing -> return False
           "stop" -> do
             let node = undefined -- mkDeregisterNode $ json
             lift $ deregisterNode consulClient node
             return True                  
           _ -> return False
    return r

event :: Socket -> IO ByteString
event s = do
  sendAll s "GET /events HTTP/1.1\r\nContent-Type: application/json\r\n\r\n"
  j <- recv s 4096
  return j

unixSocket :: IO Socket
unixSocket = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s $ SockAddrUnix "/var/run/docker.sock"
  return s

mkRegisterNodes :: Maybe ConsulStartResponse -> Maybe [RegisterNode]
mkRegisterNodes (Just res) = let name = _csrName res
                                 name' = replace "_" "-" (dropWhile (\c -> c == '/') name)
                                 net = _csrNetworkSettings res :: ConsulStartNetworkSettings
                                 ip = _csnsIPAddress net
                                 datacenter = Just $ Datacenter "dev"
                                 service = mkService res
                             in Just $ [RegisterNode datacenter name' ip (Just service) Nothing]
mkRegisterNodes Nothing = Nothing

mkDeregisterNode :: ConsulStartResponse -> DeregisterNode
mkDeregisterNode res = let name = _csrName res
                           name' = replace "_" "-" (dropWhile (\c -> c == '/') name)
                       in DeregisterNode (Just $ Datacenter "dev") name'
                                   
mkService :: ConsulStartResponse -> Service
mkService res = let cid = _csrId res
                    name = _csrName res
                    net = _csrNetworkSettings res
                    ports = _csnsPorts net
                    name' = replace "_" "-" (dropWhile (\c -> c == '/') name)
                    sid = append cid $ append "-" name'
                in Service sid name' (Map.keys ports) Nothing (Just 8080)

port2int :: Text -> Maybe Int
port2int sport = do
  let p = head $ splitOn "/" sport
      port = decimal p
  case port of
    Right (p', _) -> Just p'
    Left _ -> Nothing
              
