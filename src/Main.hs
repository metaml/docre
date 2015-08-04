{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (putStrLn, getLine, words, lines, concat, length, drop, dropWhile, null, lookup)
import Network.Socket hiding (recv)
-- import qualified GHC.IO.Exception as Ex
-- import Control.Exception (try, throwIO)
import qualified Data.HashMap.Strict as Map
import System.Directory (doesFileExist)    
import Control.Monad (forever)
import Control.Concurrent (threadDelay)    
import Control.Monad (when)
import Control.Monad.Trans.Either (left, runEitherT)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson (Object, decodeStrict)
import Data.Foldable (forM_)
import Data.Either (rights)
import Data.ByteString (ByteString, concat)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text (append, dropWhile, replace, splitOn)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Read (decimal)
import Text.Regex (mkRegex, splitRegex)
import Pipes (Consumer, Producer, Pipe, (>->), await, yield, runEffect, lift)
import Data.Docker (Event(..), StartResponse(..), StartNetworkSettings(..))
import Data.Consul (RegisterNode(..), DeregisterNode(..), Service(..), Datacenter(..))
import Network.Consul.DockerClient (registerNode, deregisterNode, mkConsulClient)

main :: IO ()
main = do
  e <- runEitherT $ forever $ do
         sock <- lift $ doesFileExist "/var/run/docker.sock"
         lift $ print sock
         _ <- lift $ threadDelay 1000000
         when (sock == True) $ left ()
  case e of
    Right _ -> main
    Left  _ -> dockerListener
    
dockerListener :: IO ()
dockerListener = runEffect $ docker >-> json2event >-> event2id >-> id2container >-> container2consul >-> consul

type Status = ByteString
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
               case (decodeStrict $ pack j) :: Maybe Event of
                 Just ev -> yield ev
                 Nothing -> lift $ putStr "error in json2event j=:" >> print j

event2id :: Pipe Event (ByteString, Status) IO ()
event2id = forever $ do
             e <- await
             lift $ putStr "event2id: e=" >> print e                  
             yield $ (encodeUtf8 $ _eId e, encodeUtf8 $ _eStatus e)

id2container :: Pipe (ByteString, Status) (ByteString, Status) IO ()
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
             let nodes = mkRegisterNodes $ decodeStrict json
             lift $ putStr "consul: nodes=" >> print  nodes
             case nodes of
               Just ns -> forM_ ns (\n -> lift $ putStr "n=" >> print n >> registerNode consulClient n) >> return True
               Nothing -> return False
           "stop" -> do
             -- let node = mkDeregisterNode $ 
             -- lift $ deregisterNode consulClient node
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

mkDeregisterNode :: StartResponse -> DeregisterNode
mkDeregisterNode res = let name' = _srName res
                           name = replace "_" "-" (dropWhile (\c -> c == '/') name')
                       in DeregisterNode (Just $ Datacenter "dev") name

-- @todo: test docker containers with and without ports (external services)

mkRegisterNodes :: Maybe StartResponse -> Maybe [RegisterNode]
mkRegisterNodes (Just res) = let name = replace "_" "-" (dropWhile (\c -> c == '/') (_srName res))
                                 dc = Just $ Datacenter "dev"
                                 net = _srNetworkSettings res :: StartNetworkSettings
                                 ip = _snsIPAddress net
                                 ps = ports (_snsPorts net)
                             in case ps of
                                  Just ps' -> Just $ map (\port -> RegisterNode dc name ip (mkService res port) Nothing) ps'
                                  Nothing -> Just [RegisterNode dc name ip (mkService res 0) Nothing]
mkRegisterNodes Nothing = Nothing

mkService :: StartResponse -> Int -> Maybe Service
mkService res port = let cid = _srId res
                         net = _srNetworkSettings res
                         name = replace "_" "-" (dropWhile (\c -> c == '/') (_srName res))
                         sid = append cid $ append "-" $ append name $ append "-" (decodeUtf8 (pack $ show port))
                         ps = _snsPorts net
                     in case ps of
                          Just ps' -> Just (Service sid name (Map.keys ps') Nothing (Just port))
                          Nothing -> Just (Service sid name [] Nothing Nothing)

ports :: Maybe Object -> Maybe [Int]
ports o = case o of
            Just obj -> let ps = Map.keys obj
                            ports' = map (\p -> decimal p) $ map (\p -> head (splitOn "/" p)) ps
                        in return $ map (\pair -> fst pair) (rights ports')
            Nothing -> Nothing
