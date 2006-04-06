{-# OPTIONS -fffi -fglasgow-exts #-}
{- |
   Module      :  System.Posix.Poll
   Copyright   :  (c) 2006-04-06 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   A foreign function interface to the POSIX system call
   @poll(2)@. Your program should link the threaded
   runtime-system when using this module in blocking
   fashion.
 -}

module System.Posix.Poll where

import Foreign
import Foreign.C
import System.Posix.Types

#include <sys/poll.h>

-- |The marshaled version of:
--
-- > struct pollfd
-- >   {
-- >   int fd;           /* file descriptor */
-- >   short events;     /* requested events */
-- >   short revents;    /* returned events */
-- >   };

data Pollfd = Pollfd Fd CShort CShort
            deriving (Show)

instance Storable Pollfd where
    sizeOf _    = #{size struct pollfd}
    alignment _ = alignment (undefined :: CInt)

    peek p = do
      fd <- #{peek struct pollfd, fd} p
      e  <- #{peek struct pollfd, events} p
      re <- #{peek struct pollfd, revents} p
      return $ Pollfd fd e re

    poke p (Pollfd fd e re) = do
      #{poke struct pollfd, fd} p fd
      #{poke struct pollfd, events} p e
      #{poke struct pollfd, revents} p re

-- |Marshaled 'Enum' representing the various @poll(2)@
-- flags.

data PollFlag
  = PollIn     -- ^ there is data to read
  | PollPri    -- ^ there is urgent data to read
  | PollOut    -- ^ writing now will not block
  | PollErr    -- ^ error condition
  | PollHup    -- ^ hung up
  | PollNVal   -- ^ invalid request: fd not open
  deriving (Eq, Bounded, Show)

instance Enum PollFlag where
  toEnum #{const POLLIN}   = PollIn
  toEnum #{const POLLPRI}  = PollPri
  toEnum #{const POLLOUT}  = PollOut
  toEnum #{const POLLERR}  = PollErr
  toEnum #{const POLLHUP}  = PollHup
  toEnum #{const POLLNVAL} = PollNVal
  toEnum i = error ("PollFlag cannot be mapped to value " ++ show i)

  fromEnum PollIn    = #{const POLLIN}
  fromEnum PollPri   = #{const POLLPRI}
  fromEnum PollOut   = #{const POLLOUT}
  fromEnum PollErr   = #{const POLLERR}
  fromEnum PollHup   = #{const POLLHUP}
  fromEnum PollNVal  = #{const POLLNVAL}

-- |The system routine @poll(2)@ may block, obviously; so it
-- is declared as a \"safe\" FFI call. In the /threaded/
-- runtime-system, this means that a blocking invocation of
-- 'poll' will not block any other execution threads. Thus,
-- you should link your programs with @-threaded@ when you
-- use this module. Further details can be found at
-- <http://www.haskell.org//pipermail/glasgow-haskell-users/2005-February/007762.html>.
--
-- In the non-threaded runtime-system, using 'poll' in
-- blocking fashion /will/ block all other threads too.

foreign import ccall safe poll :: Ptr Pollfd -> CUInt -> CInt -> IO CInt
