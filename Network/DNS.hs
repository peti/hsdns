{- |
   Module      :  Network.DNS
   Copyright   :  (c) 2005-02-02 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   An asynchronous DNS resolver.
-}

module Network.DNS
  ( -- PollResolver
    Resolver
  , initResolver
  , resolveA
  , resolvePTR
  , resolveMX
    -- Network
  , HostName
    -- Network.Socket
  , HostAddress
  , ha2tpl
  , ha2ptr
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
  )
  where

import Network           ( HostName )
import Network.DNS.ADNS
import Network.DNS.PollResolver
import Network.IP.Address
