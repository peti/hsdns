{-
    Resolve a hostnames' SRV records, then show it.

    TODO: Add the ability to perform multiple requests.
-}

module Main ( main ) where

import Control.Monad            ( when )
import System.Environment       ( getArgs )
import Network                  ( PortID(..) )
import ADNS

main :: IO ()
main = do
  names <- getArgs
  when (length names /= 1) (fail "Usage: hostname")
  initResolver [Debug] $ \resolver -> do
    a <- querySRV resolver (head names)
    case a of
	Just addr -> putStrLn $ "RESULT:\n" ++ concatMap (\b -> fst b ++ ":" ++ showPortID (snd b)) addr
	_         -> fail $ "Error in SRV " ++ show (head names)

showPortID :: PortID -> String
showPortID (PortNumber p) = show p
showPortID (UnixSocket str) = str
showPortID (Service str) = str
