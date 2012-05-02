import System.Environment (getArgs)
import Network (listenOn, withSocketsDo, PortID(..), Socket, accept)
import System.IO (Handle, hSetBuffering, BufferMode(..), hGetLine, hPutStr, hPutStrLn, hClose)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BSL


main :: IO()
main = do
	args <- getArgs
	runServer (fromIntegral (read (head args) :: Int))

runServer port = withSocketsDo $ do
	socket <- listenOn $ PortNumber $ port
	socketHandler socket

socketHandler :: Socket -> IO ()
socketHandler socket = do
	(handle, host, port) <- accept socket
	hSetBuffering handle NoBuffering
	forkIO $ process handle
	socketHandler socket

process :: Handle -> IO ()
process handle = do
	l <- hGetLine handle
	let wl = words l
	case (head $ wl) of
		"GET" -> do
			let f = tail (wl !! 1)
			exists <- doesFileExist f
			if exists
				then do
					cont <- BSL.readFile f
					hPutStr handle ((wl !! 2) ++ "length:")
					hPutStr handle (show (BSL.length cont) ++ "\n")
					BSL.hPutStr handle cont
				else do
					hPutStrLn handle ("404 not found")
		_     -> do
			hPutStrLn handle ("404 not found")
	hClose handle
