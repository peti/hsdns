{-# OPTIONS -fffi -fglasgow-exts #-}
{- |
   Module      :  System.Posix.Poll
   Copyright   :  (c) 2005-02-02 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   An incomplete foreign function interface to the POSIX
   system call @poll(2)@.
-}

module System.Posix.Poll where

import Foreign
import Foreign.C

#include <poll.h>

-- |Opaque data type, provided only so that we can define
-- 'sizeOf' for it in order to be able to 'mallocArray' an
-- array for @poll(2)@. 'peek' and 'poke' are not
-- implemented.

data Pollfd

instance Storable Pollfd where
    sizeOf _    = #{size struct pollfd}
    alignment _ = alignment (undefined :: CInt)
    poke _ _    = error "poke is not defined for Pollfd"
    peek _      = error "peek is not defined for Pollfd"

-- |The system routine @poll(2)@.

foreign import ccall unsafe poll :: Ptr Pollfd -> CUInt -> CInt -> IO CInt
