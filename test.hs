-- Resolve a bunch of hostnames' A records, then resolve
-- those A-record's PTR records and check whether they
-- match. Do it all asynchronously. The results are printed
-- in the order the answers come in.

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
  initResolver [ NoErrPrint, NoServerWarn ] $ \query -> do
    rrChannel <- newChan :: IO (Chan CheckResult)
    mapM_ (\h -> forkIO (ptrCheck query rrChannel h)) names
    replicateM_ (length names) (readChan rrChannel >>= print)

ptrCheck :: Resolver -> Chan CheckResult -> HostName -> IO ()
ptrCheck query chan host = do
    a <- query host A [] >>= takeMVar
    case a of
      Answer _ _ _ _ [RRA addr@(RRAddr addr')] -> do
        ptr <- query (ha2ptr addr') PTR [] >>= takeMVar
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
-- haskell-ghci-program-args: ( "-ladns" ) ***
-- End: ***
