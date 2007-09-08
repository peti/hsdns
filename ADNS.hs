{- |
   Module      :  Network.DNS
   Copyright   :  (c) 2007 Peter Simons
   License     :  LGPL

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   An asynchronous DNS resolver. You should link your program
   with the /threaded/ runtime-system when using this module. In
   GHC, this is accomplished by specifying @-threaded@ on the
   command-line.
-}

module ADNS
  ( HostName, HostAddress
  , Resolver, initResolver, InitFlag(..)
  , queryA, queryPTR, queryMX
  , dummyDNS
  )
  where

import Network           ( HostName )
import Network.Socket    ( HostAddress )
import ADNS.Base
import ADNS.Resolver

queryA :: Resolver -> HostName -> IO (Maybe [HostAddress])
queryA = query resolveA

queryPTR :: Resolver -> HostAddress -> IO (Maybe [HostName])
queryPTR = query resolvePTR

queryMX :: Resolver -> HostName -> IO (Maybe [(HostName, HostAddress)])
queryMX = query resolveMX

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns" ***
-- End: ***
