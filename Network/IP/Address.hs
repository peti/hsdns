{- |
   Module      :  Network.IP.Address
   Copyright   :  (c) 2005-02-02 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   Tools for manipulating IP addresses.
-}

module Network.IP.Address
  ( module Network.IP.Address
  , HostAddress
  , inet_addr
  , inet_ntoa
  )
  where

import Data.Endian
import Data.Bits
import Network.Socket

-- |Split up an IP address in network byte-order.

ha2tpl :: HostAddress -> (Int, Int, Int, Int)
ha2tpl n =
  let (b1,n1) = (n  .&. 255, n  `shiftR` 8)
      (b2,n2) = (n1 .&. 255, n1 `shiftR` 8)
      (b3,n3) = (n2 .&. 255, n2 `shiftR` 8)
      b4      = n3 .&. 255
  in
  case ourEndian of
    BigEndian    -> (fromEnum b4, fromEnum b3, fromEnum b2, fromEnum b1)
    LittleEndian -> (fromEnum b1, fromEnum b2, fromEnum b3, fromEnum b4)
    PDPEndian    -> (fromEnum b4, fromEnum b3, fromEnum b1, fromEnum b2)

-- |Turn a 32-bit IP address into a string suitable for
-- 'PTR' lookups in the Domain Name System.

ha2ptr :: HostAddress -> String
ha2ptr n = shows b4 . ('.':) .
           shows b3 . ('.':) .
           shows b2 . ('.':) .
           shows b1 $ ".in-addr.arpa."
  where
  (b1,b2,b3,b4) = ha2tpl n
