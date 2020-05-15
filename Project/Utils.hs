module Project.Utils(stringToWord8, stringToPortNumber, listToHostTuple, parseIp) where
import GHC.Word
import Network.Socket
import Data.List.Split

stringToWord8::String->Word8
stringToWord8 x = read x :: Word8

stringToPortNumber::String->PortNumber
stringToPortNumber x = read x :: PortNumber

listToHostTuple::(String->Word8)->[String]->(Word8, Word8, Word8, Word8)
listToHostTuple f [w, x, y, z] = (f w, f x, f y, f z)

parseIp::String->[String]
parseIp ip = splitOn "." ip