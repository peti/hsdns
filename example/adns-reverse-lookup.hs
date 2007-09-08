-- Resolve a bunch of hostnames' A records, then resolve
-- those A-record's PTR records and check whether they
-- match. Do it all asynchronously. The results are printed
-- in the order the answers come in:
--
--   $ ghc -threaded --make test.hs -o test -ladns
--   $ ./test xyz.example.org ecrc.de www.example.com www.cryp.to
--   DNSError "xyz.example.org: can't resolve A:" nxdomain
--   NotOK "www.cryp.to" 195.234.152.69 "research.cryp.to"
--   NotOK "ecrc.de" 127.0.0.1 "localhost"
--   OK "www.example.com" 192.0.34.166

module Main ( main ) where

import Control.Monad            ( when, replicateM_ )
import Control.Concurrent       ( forkIO )
import Control.Concurrent.Chan  ( Chan, newChan, writeChan, readChan )
import System.Environment       ( getArgs )
import Network.Socket           ( inet_ntoa )
import Foreign                  ( unsafePerformIO )
import ADNS

data CheckResult
  = OK HostName HostAddress
  | NotOK HostName HostAddress HostName
  | DNSError String

showAddr :: HostAddress -> String
showAddr = unsafePerformIO . inet_ntoa

instance Show CheckResult where
  showsPrec _ (OK h a)       = showString $ "OK: "   ++ h ++ " <-> " ++ showAddr a
  showsPrec _ (NotOK h a h') = showString $ "FAIL: " ++ h ++ " -> "  ++ showAddr a ++ " -> " ++ h'
  showsPrec _ (DNSError msg) = showString $ "ERR: " ++ msg

main :: IO ()
main = do
  names <- getArgs
  when (null names) (print "Usage: hostname [hostname ...]")
  initResolver [NoErrPrint, NoServerWarn] $ \resolver -> do
    rrChannel <- newChan :: IO (Chan CheckResult)
    mapM_ (\h -> forkIO (ptrCheck resolver rrChannel h)) names
    replicateM_ (length names) (readChan rrChannel >>= print)

ptrCheck :: Resolver -> Chan CheckResult -> HostName -> IO ()
ptrCheck resolver chan host = do
  let returnError t = writeChan chan (DNSError (host ++ ": cannot resolve " ++ t))
  a <- queryA resolver host
  case a of
    Just [addr] -> do
      ptr <- queryPTR resolver addr
      case ptr of
        Just [name] | name == host -> writeChan chan (OK host addr)
                    | otherwise    -> writeChan chan (NotOK host addr name)
        _                          -> returnError "PTR"
    _           -> returnError "A"



-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns" ***
-- End: ***
