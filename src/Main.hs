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
import Data.Aeson.Types (Object, Value(..))
import Data.ByteString (concat, ByteString)
import Data.ByteString.Char8 (putStrLn, pack, unpack)
import Data.Text (dropWhile, replace)
import Data.Text.Encoding (encodeUtf8)
import Text.Regex (mkRegex, splitRegex)
import Pipes ((>->), await, yield, runEffect, lift, Consumer, Producer, Pipe)
import Data.Docker (Event(..))
import Data.Consul (RegisterNode(..), DeregisterNode(..), Service(..), Datacenter(..))    
import Network.Consul.DockerClient (registerNode, deregisterNode, mkConsulClient)
import Control.Concurrent (threadDelay)    
import Control.Monad (when)
import Control.Monad.Trans ()
import Control.Monad.Trans.Either

main :: IO ()
main = do
  e <- runEitherT $ forever $ do
         sock <- lift $ doesFileExist "/var/run/docker.sock"
         _ <- lift $ threadDelay 1000000
         when (sock) $ left ()
  case e of
    Left  _ -> main
    Right _ -> dockerListener
    
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

container2consul :: Pipe (Cid, Status) (Object, Status) IO ()
container2consul = forever $ do
                     (r, status) <- await  -- r: http response
                     let o :: Maybe Object = decodeStrict json where
                                               json = pack $ last $ init $ filter (\c -> c /= "")
                                                      (splitRegex (mkRegex "[ \t\r\n]+") (unpack r))
                     lift $ print o                          
                     case o of
                       Nothing -> lift $ putStrLn "error in container2consul"
                       Just e -> yield (e, status)
                                 
consul :: Consumer (Object, Status) IO ()
consul = do
  consulClient <- mkConsulClient
  forever $ do
    (h, status) <- await
    let (Just (String cid)) = Map.lookup "Id" h
        (Just (String name)) = Map.lookup "Name" h
        Just net = Map.lookup "NetworkSettings" h
        (Just (String ip)) = ipAddress net
        name' = replace "_" "-" (dropWhile (\c -> c == '/') name)
    r <- case status of
           "start" -> do
             let service = Service cid name' ["http"] Nothing Nothing
                 node = RegisterNode (Just $ Datacenter "dev") name' ip (Just service) Nothing
             lift $ registerNode consulClient node
           "stop" -> do
             let node = DeregisterNode (Just $ Datacenter "dev") name'
             lift $ deregisterNode consulClient node
           _ -> return False
    lift $ print status >> print cid >> print name >> print ip
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

ipAddress :: Value -> Maybe Value
ipAddress (Object obj) = Map.lookup "IPAddress" obj
ipAddress _ = Nothing
