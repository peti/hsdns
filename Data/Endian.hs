{- |
   Module      :  Data.Endian
   Copyright   :  (c) 2005-02-04 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  stable
   Portability :  portable

   Find out the machine's endian at runtime.
-}

module Data.Endian ( Endian(..), ourEndian ) where

import Foreign

-- |Definitions for byte order according to significance of
-- bytes from low addresses to high addresses.

data Endian
  = LittleEndian                -- ^ byte order: @1234@
  | BigEndian                   -- ^ byte order: @4321@
  | PDPEndian                   -- ^ byte order: @3412@
  deriving (Show, Eq)

-- |The endian of this machine, determined at run-time.

{-# NOINLINE ourEndian #-}
ourEndian :: Endian
ourEndian =
  unsafePerformIO $
    allocaArray (sizeOf (undefined :: Word32)) $ \p -> do
      let val = 0x01020304 :: Word32
      poke p val
      let p' = castPtr p :: Ptr Word8
      val' <- peekArray 4 p'
      case val' of
        (0x01:0x02:0x03:0x04:[]) -> return BigEndian
        (0x04:0x03:0x02:0x01:[]) -> return LittleEndian
        (0x02:0x01:0x03:0x04:[]) -> return PDPEndian
        _                        -> error "unknown endian"
