{-# OPTIONS -fffi -fglasgow-exts #-}
{- |
   Module      :  System.Posix.GetTimeOfDay
   Copyright   :  (c) 2005-02-02 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   A foreign function interface to @gettimeofday(2)@.
-}

module System.Posix.GetTimeOfDay where

import Foreign
import Foreign.C

#include <sys/time.h>

-- |Marshaling for C's @struct timeval@.

data Timeval = Timeval CTime #{type suseconds_t}

-- |Not really implemented by anyone; so we provide just a
-- place-holder.

data Timezone

instance Storable Timeval where
  sizeOf _    = #{size struct timeval}
  alignment _ = alignment (undefined :: CTime)
  poke ptr (Timeval t us)
              = do #{poke struct timeval, tv_sec} ptr t
                   #{poke struct timeval, tv_usec} ptr us
  peek ptr    = do t <- #{peek struct timeval, tv_sec} ptr
                   us <- #{peek struct timeval, tv_usec} ptr
                   return (Timeval t us)

-- |Write the current time of the day as a 'Timeval'. The
-- time is returned in local time, no time zone correction
-- takes place. Signals errors with 'throwErrno'.

getTimeOfDay :: Ptr Timeval -> IO ()
getTimeOfDay p  = do
  rc <- gettimeofday p nullPtr
  case rc of
    0 -> return ()
    _ -> throwErrno "GetTimeOfDay"

-- |The @gettimeofday(2)@ system call.

foreign import ccall unsafe gettimeofday
  :: Ptr Timeval -> Ptr Timezone -> IO CInt
