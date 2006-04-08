{- |
   Module      :  Network.DNS
   Copyright   :  (c) 2006-04-08 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   An asynchronous DNS resolver. Link your program with the
   /threaded/ runtime-system when you use this module. In
   GHC, this is accomplished by specifying @-threaded@ on
   the command-line.
-}

module Network.DNS
  ( -- PollResolver
    Resolver
  , initResolver
  , resolveA
  , resolvePTR
  , resolveMX
  , query
    -- Network
  , HostName
    -- Network.Socket
  , HostAddress
    -- ADNS
  , InitFlag(..)
  , QueryFlag(..)
  , RRType(..)
  , Status(..)
  , RRAddr(..)
  , RRHostAddr(..)
  , RRIntHostAddr(..)
  , Answer(..)
  , Response(..)
  , sOK
  , sNOMEMORY
  , sUNKNOWNRRTYPE
  , sSYSTEMFAIL
  , sMAX_LOCALFAIL
  , sTIMEOUT
  , sALLSERVFAIL
  , sNORECURSE
  , sINVALIDRESPONSE
  , sUNKNOWNFORMAT
  , sMAX_REMOTEFAIL
  , sRCODESERVFAIL
  , sRCODEFORMATERROR
  , sRCODENOTIMPLEMENTED
  , sRCODEREFUSED
  , sRCODEUNKNOWN
  , sMAX_TEMPFAIL
  , sINCONSISTENT
  , sPROHIBITEDCNAME
  , sANSWERDOMAININVALID
  , sANSWERDOMAINTOOLONG
  , sINVALIDDATA
  , sMAX_MISCONFIG
  , sQUERYDOMAINWRONG
  , sQUERYDOMAININVALID
  , sQUERYDOMAINTOOLONG
  , sMAX_MISQUERY
  , sNXDOMAIN
  , sNODATA
  , sMAX_PERMFAIL
  , adnsStrerror
  , adnsErrAbbrev
  , adnsErrTypeAbbrev
  , dummyDNS
  )
  where

import Network           ( HostName )
import Network.DNS.ADNS
import Network.DNS.PollResolver
import Network.IP.Address
