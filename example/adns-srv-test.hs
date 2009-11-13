{-
    Resolve a hostnames' SRV records, then show it
-}

module Main ( main ) where

import Control.Monad            ( when )
import System.Environment       ( getArgs )
import Network.Socket           ( inet_ntoa )
import Data.List                ( elem )
import ADNS

main :: IO ()
main = do
  names <- getArgs
  when (null names) (putStrLn "Usage: hostname [hostname ...]")
  initResolver [Debug] $ \resolver -> do
    a <- querySRV resolver $ head names
    case a of
	Just addr -> do
	    putStrLn $ "RESULT:\n" ++ (concat $ map (\b -> (fst b) ++ (show $ snd b) ++ "\n") addr)
	_ -> putStrLn $ "Error in SRV " ++ (show a)

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns" ***
-- End: ***
