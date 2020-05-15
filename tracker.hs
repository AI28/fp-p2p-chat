{-# LANGUAGE OverloadedStrings #-}

import qualified Database.MongoDB as Mongo
import Network.Socket
import System.IO
import Data.List.Split

main::IO ()
main = do
        acceptConnection (3334,  tupleToHostAddress (127,0,0,1))
        e <- retrievePeers
        print e


acceptConnection::(PortNumber, HostAddress)-> IO Mongo.Value
acceptConnection (pn, ha) = do
                            sock <- socket AF_INET Stream 0
                            setSocketOption sock KeepAlive 1
                            bind sock $ SockAddrInet pn 0
                            listen sock 5
                            conn <- accept sock
                            runConnection conn

runConnection::(Socket, SockAddr) -> IO Mongo.Value
runConnection (sock, sock_addr) = do
                                  socket_name <- getPeerName sock
                                  putStrLn $ show socket_name
                                  hdl <- socketToHandle sock ReadWriteMode
                                  hSetBuffering hdl NoBuffering
                                  peer_name <- hGetLine hdl
                                  insertPeer (head ( splitOn ":" peer_name)) (head ( splitOn ":"  ( show socket_name)),(last ( splitOn ":" peer_name)))
retrievePeers::IO [Mongo.Document]
retrievePeers = do
                pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
                let run act = Mongo.access pipe Mongo.master "test" act
                run $ Mongo.find (Mongo.select [] "posts") >>= Mongo.rest

insertPeer::String->(String, String)-> IO Mongo.Value
insertPeer peer_name (peer_address, peer_port) =do
                                                pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
                                                let run act = Mongo.access pipe Mongo.master "test" act
                                                run  $ Mongo.insert "posts" ["name" Mongo.=: peer_name, "peerAddress" Mongo.=: peer_address, "peerPort" Mongo.=: peer_port]
