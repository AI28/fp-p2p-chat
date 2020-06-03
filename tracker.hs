{-# LANGUAGE OverloadedStrings #-}
import qualified Database.MongoDB as Mongo
import Network.Socket
import System.IO
import Data.List.Split
import Control.Concurrent
import Project.Utils
import System.Environment

main::IO ()
main = do
        args <- getArgs
        acceptConnection (stringToPortNumber $ head args,  tupleToHostAddress (127,0,0,1))

acceptConnection::(PortNumber, HostAddress)->IO()
acceptConnection (pn, ha) = do
                            sock <- socket AF_INET Stream 0
                            setSocketOption sock KeepAlive 1
                            bind sock $ SockAddrInet pn 0
                            listen sock 5
                            listeningLoop sock

listeningLoop::Socket->IO()
listeningLoop sock = do
                       conn <- accept sock
                       forkIO $ runConnection conn
                       listeningLoop sock
                           
runConnection::(Socket, SockAddr)->IO()
runConnection (sock, sock_addr) = do
                                  socket_name <- getPeerName sock
                                  putStrLn $ show socket_name
                                  hdl <- socketToHandle sock ReadWriteMode
                                  hSetBuffering hdl NoBuffering
                                  request <- hGetLine hdl
                                  peerResponse hdl request

peerResponse::Handle->String->IO()
peerResponse handle request
                                             | head ( splitOn ":" request ) == "r" = do
                                                                                      insertPeer (head $ tail $ splitOn ":" request) (head $ tail $ tail $ splitOn ":" request)  (last $ splitOn ":" request)
                                                                                      hPutStrLn handle "r:You have been registered succesfully."
                                             | head ( splitOn ":" request ) == "g" = do
                                                                                     peers <- retrievePeers
                                                                                     hPutStrLn handle $ show peers
                                             | otherwise = hPutStrLn handle "Please insert a valid request."

retrievePeers::IO [Mongo.Document]
retrievePeers = do
                pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
                let run act = Mongo.access pipe Mongo.master "test" act
                run $ (Mongo.find (Mongo.select [] "posts") {Mongo.project = ["_id" Mongo.=: (0::Int)]}) >>= Mongo.rest

insertPeer::String->String->String-> IO Mongo.Value
insertPeer peer_name peer_address peer_port =do
                                                pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
                                                let run act = Mongo.access pipe Mongo.master "test" act
                                                run  $ Mongo.insert "posts" ["name" Mongo.=: peer_name, "peerAddress" Mongo.=: peer_address, "peerPort" Mongo.=: peer_port]
