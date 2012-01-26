import System.Environment (getArgs)
import Network (listenOn, withSocketsDo, PortID(..), Socket)


main :: IO()
main = do
	args <- getArgs
	runServer fromIntegral (read $ (head args :: Int))

runServer port = withSocketsDo $ do
	socket <- listenOn $ PortNumber $ port
	socketHandler socket

socketHandler :: Socket -> IO ()
