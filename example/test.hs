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

module Main where

import Control.Monad           ( when, replicateM_ )
import Control.Concurrent      ( forkIO )
import Control.Concurrent.MVar ( takeMVar )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan )
import System.Environment      ( getArgs )
import Network.DNS
import Network.IP.Address

data CheckResult
  = OK HostName RRAddr
  | NotOK HostName RRAddr HostName
  | DNSError String Answer
  deriving (Show)

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
    a <- resolver host A [] >>= takeMVar
    case a of
      Answer _ _ _ _ [RRA addr@(RRAddr addr')] -> do
        ptr <- resolver (ha2ptr addr') PTR [] >>= takeMVar
        case ptr of
          Answer _ _ _ _ [RRPTR name] ->
            if (name == host)
               then writeChan chan (OK host addr)
               else writeChan chan (NotOK host addr name)

          _ -> writeChan chan (DNSError (host++": can't resolve PTR:") ptr)

      _ -> writeChan chan (DNSError (host++": can't resolve A:") a)


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns -lcrypto" ***
-- End: ***