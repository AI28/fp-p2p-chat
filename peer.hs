import Network.Socket
import System.IO
import System.Environment
import GHC.Word
import Data.List.Split
import Project.Utils
import Control.Concurrent

main :: IO ()
main = do
       putStrLn "Insert w if you are waiting for a peer to contact you or m [PORT NUMBER] to contact a peer yourself."
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
                               putStrLn "Please insert the tracker's port and ip address and the port that you will listen on."
                               port <- getLine
                               ip_address <- getLine
                               my_port <- getLine
                               putStrLn "Please insert a username."
                               username <- getLine
                               registerAsPeer username my_port (port, ip_address)
                  | x == "h" = do
                               putStrLn "m \n PORT \n IPv4 = message a peer ; w = wait to be messaged by a peer ; r \n PORT \n IPv4 = register peer with tracker"

                  | otherwise = do
                                putStrLn "error: please type w if you wish to wait for a peer to contact you or m [PORT_NUMBER] if you wish to message someone."

registerAsPeer::String->String->(String, String)->IO()
registerAsPeer username my_port (pn, ha) = do
                                           sock <- socket AF_INET Stream 0
                                           setSocketOption sock KeepAlive 1
                                           connect sock $ SockAddrInet (stringToPortNumber pn) (tupleToHostAddress (listToHostTuple stringToWord8 (parseIp ha)))
                                           hdl <- socketToHandle sock ReadWriteMode
                                           hPutStrLn hdl (username++":"++my_port)
acceptConn::(PortNumber, HostAddress)->IO ()
acceptConn (pn, ha) = do
                  sock <- socket AF_INET Stream 0
                  setSocketOption sock KeepAlive 1
                  bind sock $ SockAddrInet pn 0
                  listen sock 1
                  conn <- accept sock
                  runIncomingConn conn

connectToPeer::(PortNumber, HostAddress)->IO ()
connectToPeer (pn, ha) = do
                         outgoing_sock <- socket AF_INET Stream 0
                         setSocketOption outgoing_sock KeepAlive 1
                         connect outgoing_sock $ SockAddrInet pn ha
                         runOutgoingConn (outgoing_sock, (SockAddrInet pn ha))
                       
runOutgoingConn::(Socket, SockAddr) -> IO ()
runOutgoingConn (sock, sock_addr) = do
    socket_name <- getPeerName sock -- preluam adresa ip si portul sursa al ultimului pachet primit
    putStrLn $ show socket_name
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    outgoingHandler hdl

outgoingHandler::Handle->IO()
outgoingHandler hdl = do
                      text <- getLine
                      hPutStrLn hdl text
                      text2 <- hGetLine hdl
                      putStrLn ("Peer:"++text2)
                      outgoingHandler hdl

runIncomingConn::(Socket, SockAddr)->IO()
runIncomingConn (sock, sock_addr) = do
                            socket_name <- getPeerName sock
                            putStrLn $ show socket_name
                            hdl <- socketToHandle sock ReadWriteMode
                            hSetBuffering hdl NoBuffering
                            incomingHandler hdl

incomingHandler::Handle->IO()
incomingHandler hdl = do
                      text <- hGetLine hdl
                      putStrLn ("Peer: " ++ text)
                      text2 <- getLine
                      hPutStrLn hdl text2
                      incomingHandler hdl