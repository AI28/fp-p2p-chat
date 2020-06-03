import Network.Socket
import System.IO
import System.Environment
import GHC.Word
import Data.List.Split
import Project.Utils
import Control.Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do
       command <- getLine
       processCommand command
       main

processCommand::String->IO ()
processCommand x  
                  | x == "m" = do
                               putStrLn "Please insert your peer's port."
                               port <- getLine
                               putStrLn "Please insert your peer's IPv4 address"
                               ip_v4 <- getLine
                               connectToPeer (stringToPortNumber port, tupleToHostAddress ( listToHostTuple stringToWord8 (parseIp ip_v4)))
                  | x == "w" = do
                               putStrLn "Please insert your own port."
                               port <- getLine
                               acceptConn (stringToPortNumber port, tupleToHostAddress (127, 0, 0, 1))
                  | x == "r" = do
                               putStrLn "Please insert the tracker's port."
                               port <- getLine
                               putStrLn "Please insert the tracker's ip address."
                               ip_address <- getLine
                               putStrLn "Please insert the ip address on which you will listen on."
                               my_ip_address <- getLine
                               putStrLn "Please insert the port on which you are listening on."
                               my_port <- getLine
                               putStrLn "Please insert a username."
                               username <- getLine
                               registerAsPeer username my_port (my_ip_address) (stringToPortNumber port, tupleToHostAddress (listToHostTuple stringToWord8 (parseIp ip_address)))
                  | x == "g" = do
                               putStrLn "Insert the tracker's ip address"
                               ip <- getLine
                               putStrLn "Insert the tracker's port"
                               port <- getLine
                               getPeers (stringToPortNumber port, tupleToHostAddress (listToHostTuple stringToWord8 (parseIp ip)))
                  | x == "nt" = do
                               putStrLn "Insert the new tracker's ip address."
                               ip_address <- getLine
                               putStrLn "Insert the new tracker's port."
                               tracker_port <- getLine
                               appendFile "./trackers" ("\n"++ip_address++":"++tracker_port)
                               putStrLn "Tracker succesfully registered."
                  | x == "gt" = do
                               contents <- readFile "./trackers"
                               putStrLn contents
                               putStrLn "Trackers list."
                  | x == "h" = do
                               putStrLn " m = message a peer \n w = wait to be messaged by a peer \n r = register peer with tracker \n g = get peers from tracker \n nt = insert a new tracker \n gt = get trackers list"

                  | otherwise = do
                                putStrLn "error: unrecognised command. type h to see the commands list"

getPeers::(PortNumber, HostAddress)->IO()
getPeers (pn, ha) = do
                      sock <- socket AF_INET Stream 0
                      setSocketOption sock KeepAlive 1
                      connect sock $ SockAddrInet pn ha
                      hdl <- socketToHandle sock ReadWriteMode
                      hPutStrLn hdl "g:"
                      result <- hGetLine hdl
                      putStrLn result

registerAsPeer::String->String->String->(PortNumber, HostAddress)->IO()
registerAsPeer username my_port ip_address (pn, ha) = do
                                           sock <- socket AF_INET Stream 0
                                           setSocketOption sock KeepAlive 1
                                           connect sock $ SockAddrInet pn ha
                                           hdl <- socketToHandle sock ReadWriteMode
                                           hPutStrLn hdl ("r:"++username++":"++ip_address++":"++my_port)

acceptConn::(PortNumber, HostAddress)->IO ()
acceptConn (pn, ha) = do
                  sock <- socket AF_INET Stream 0
                  setSocketOption sock KeepAlive 1
                  bind sock $ SockAddrInet pn 0
                  listen sock 1
                  conn <- accept sock
                  runConnection conn outgoingHandler

connectToPeer::(PortNumber, HostAddress)->IO ()
connectToPeer (pn, ha) = do
                         outgoing_sock <- socket AF_INET Stream 0
                         setSocketOption outgoing_sock KeepAlive 1
                         connect outgoing_sock $ SockAddrInet pn ha
                         runConnection (outgoing_sock, (SockAddrInet pn ha)) (incomingHandler)
                       
outgoingHandler::Handle->IO()
outgoingHandler hdl = do
                      text <- getLine
                      hPutStrLn hdl text
                      outgoingHandler hdl

incomingHandler::Handle->IO()
incomingHandler hdl = do
                      text <- hGetLine hdl
                      putStrLn ("Peer: " ++ text)
                      incomingHandler hdl

runConnection::(Socket, SockAddr)->(Handle->IO())->IO()
runConnection (sock, sock_addr) appropriate_handler = do
                                                        socket_name <- getPeerName sock
                                                        putStrLn $ show socket_name
                                                        hdl <- socketToHandle sock ReadWriteMode
                                                        hSetBuffering hdl NoBuffering
                                                        async $ outgoingHandler hdl
                                                        incomingHandler hdl
