{- |
   Module      :  Network.DNS.PollResolver
   Copyright   :  (c) 2005-02-06 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   This module provides a 'poll'-based I\/O scheduler for
   "Network.DNS.ADNS". See the @test.hs@ program included in
   the distribution for an example of how to use this
   resolver. Link your program with the /threaded/
   runtime-system when you use this module. In GHC, this is
   accomplished by specifying @-threaded@ on the
   command-line.
 -}

module Network.DNS.PollResolver where

import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar
import Control.Monad      ( when )
import Data.List          ( sortBy )
import Foreign
import Foreign.C
import Network            ( HostName )
import Network.IP.Address
import Network.DNS.ADNS
import System.Posix.Poll
import System.Posix.GetTimeOfDay

-- * Resolver API

-- |A 'Resolver' is an 'IO' computation which -- given the
-- name and type of the record to query -- returns an 'MVar'
-- that will eventually contain the 'Answer' from the Domain
-- Name System.

type Resolver = String -> RRType -> [QueryFlag] -> IO (MVar Answer)

-- |Run the given 'IO' computation with an Initialized
-- 'Resolver'. Note that resolver functions can be shared,
-- and /should/ be shared between any number of 'IO'
-- threads. You may use multiple resolvers, of course, but
-- doing so defeats the purpose of an asynchronous resolver.

initResolver :: [InitFlag] -> (Resolver -> IO a) -> IO a
initResolver flags f = do
  adnsInit flags $ \dns -> do
    fds <- mallocForeignPtrArray initSize
    mst <- newMVar (RState dns fds initSize [])
    f (resolve mst)
  where
  initSize = 32

-- |Resolve a hostname's 'A' records.

resolveA :: Resolver -> HostName -> IO (Either Status [HostAddress])
resolveA resolver x = do
  Answer rc _ _ _ rs  <- resolver x A [] >>= takeMVar
  if rc /= sOK
     then return (Left rc)
     else return (Right [ addr | RRA (RRAddr addr) <- rs  ])

-- |Get the 'PTR' records assigned to a host address. Note
-- that although the API allows for a record to have more
-- than one 'PTR' entry, this will actually not happen
-- because the GNU adns library can't handle this case and
-- will return 'sINCONSISTENT'.

resolvePTR :: Resolver -> HostAddress -> IO (Either Status [HostName])
resolvePTR resolver x = do
  Answer rc _ _ _ rs  <- resolver (ha2ptr x) PTR [] >>= takeMVar
  if rc /= sOK
     then return (Left rc)
     else return (Right [ addr | RRPTR addr <- rs  ])

-- |Resolve the mail exchangers for a hostname. The returned
-- list may contain more than one entry per hostname, in
-- case the host has several 'A' records. The records are
-- returned in the order you should try to contact them as
-- determined by the priority in the 'RRMX' response.

resolveMX :: Resolver -> HostName -> IO (Either Status [(HostName, HostAddress)])
resolveMX resolver x = do
  Answer rc _ _ _ rs  <- resolver x MX [] >>= takeMVar
  if rc /= sOK
     then return (Left rc)
     else do
       let cmp (RRMX p1 _) (RRMX p2 _) = compare p1 p2
           cmp _ _= error $ showString "unexpected record in MX lookup: " (show rs)
           rs' = sortBy cmp rs
           as   = [ (hn,a) | RRMX _ (RRHostAddr hn stat has) <- rs'
                           , stat == sOK && not (null has)
                           , RRAddr a <- has ]
       return (Right as)

-- |Convenience wrapper that will modify any of the
-- @revolveXXX@ functions above to return 'Maybe' rather
-- than 'Either'. The idea is that @Nothing@ signifies any
-- sort of failure; @Just []@ signifies 'sNXDOMAIN'; and
-- everything else signifies 'sOK'.
--
-- So if you aren't interested in getting accurate 'Status'
-- codes in case of failures. Wrap your DNS queries as
-- follows:
--
-- > queryA :: Resolver -> HostName -> IO (Maybe [HostAddress])
-- > queryA = query resolveA

query :: (Resolver -> a -> IO (Either Status [b]))
      -> (Resolver -> a -> IO (Maybe [b]))
query f dns x = fmap toMaybe (f dns x)
  where
  toMaybe (Left rc)
    | rc == sNXDOMAIN  = Just []
    | otherwise        = Nothing
  toMaybe (Right r)    = Just r

-- |Use this function to disable DNS resolving. It will
-- always return @('Answer' 'sSYSTEMFAIL' Nothing (Just
-- host) (-1) [])@.

dummyDNS :: Resolver
dummyDNS host _ _ = newMVar
  (Answer sSYSTEMFAIL Nothing (Just host) (-1) [])

-- * Implementation

-- |The internal state of the resolver is stored in an
-- 'MVar' so that it is shared (and synchronized) between
-- any number of concurrent 'IO' threads.

data ResolverState = RState
  { adns     :: AdnsState               -- ^opaque ADNS state
  , pollfds  :: ForeignPtr Pollfd       -- ^array for poll(2)
  , capacity :: Int                     -- ^size of the array
  , queries  :: [(Query, MVar Answer)]  -- ^currently open queries
  }

-- |Submit a DNS query to the resolver and check whether we
-- have a running 'resolveLoop' thread already. If we don't,
-- start one with 'forkIO'. Make sure you link the threaded
-- RTS so that the main loop will not block other threads.

resolve :: MVar ResolverState -> Resolver
resolve mst r rt qfs = modifyMVar mst $ \st -> do
  res <- newEmptyMVar
  q <- adnsSubmit (adns st) r rt qfs
  when (null (queries st))
    (forkIO (resolveLoop mst) >> return ())
  let st' = st { queries = (q,res):(queries st) }
  return (st', res)

-- |Loop until all open queries have been resolved. Uses
-- 'poll' internally to avoid busy-polling the ADNS sockets.

resolveLoop :: MVar ResolverState -> IO ()
resolveLoop mst = do
  empty <- modifyMVar mst $ \(RState dns fds cap qs) -> do
    res' <- mapM (checkQuery dns) qs
    case [ x | Just x <- res' ] of
      []  -> do adnsQueries dns >>= mapM_ adnsCancel
                return ((RState dns fds cap []), True)
      res -> return ((RState dns fds cap res), False)
  when (not empty) (waitForIO >> resolveLoop mst)

  where
  checkQuery dns (q, mv) = do
   res <- adnsCheck dns q
   case res of
     Just a  -> putMVar mv a >> return Nothing
     Nothing -> return (Just (q, mv))

  waitForIO = do
    (nfds,to) <- beforePoll
    when (nfds > 0) (doPoll nfds to >> afterPoll nfds)

  beforePoll = do
    b4 <- modifyMVar mst $ \st ->
      withForeignPtr (pollfds st) $ \fds ->
        alloca $ \nfds ->
          alloca $ \to ->
            alloca $ \now -> do
              poke nfds (toEnum (capacity st))
              poke to (-1)
              getTimeOfDay now
              rc <- adnsBeforePoll (adns st) fds nfds to now
              n  <- peek nfds
              tv <- peek to
              if rc == 0 then return (st, (Right (n,tv))) else
                if (Errno rc) == eRANGE then return (st, (Left n)) else
                  fail ("adnsBeforePoll returned unknown value " ++ show rc)
    case b4 of
      Left  n -> allocFds (fromEnum n) >> beforePoll
      Right x -> return x

  doPoll nfds to = do
    fds' <- withMVar mst (return . pollfds)
    rc <- withForeignPtr fds' $ \fds ->
            poll fds (toEnum (fromEnum nfds)) to
    when (rc < 0) (throwErrno "PollResolver.doPoll failed")

  afterPoll nfds =
    withMVar mst $ \st ->
      alloca $ \now ->
        withForeignPtr (pollfds st) $ \fds -> do
          getTimeOfDay now
          adnsAfterPoll (adns st) fds nfds now

  allocFds n = modifyMVar_ mst $ \st ->
    if n <= capacity st then return st else do
      let sizes  = iterate (*2) (capacity st)
          (n':_) = dropWhile (<n) sizes
      fds <- mallocForeignPtrArray n'
      return st { pollfds  = fds
                , capacity = n'
                }

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" ) ***
-- End: ***
