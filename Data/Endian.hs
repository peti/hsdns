{- |
   Module      :  Data.Endian
   Copyright   :  (c) 2007 Peter Simons
   License     :  LGPL

   Maintainer  :  simons@cryp.to
   Stability   :  stable
   Portability :  portable

   Determine the machine's endian.
-}

module Data.Endian ( Endian(..), endian ) where

import Foreign

-- |Definitions for byte order according to significance of
-- bytes from low addresses to high addresses.

data Endian
  = LittleEndian                -- ^ byte order: @1234@
  | BigEndian                   -- ^ byte order: @4321@
  | PDPEndian                   -- ^ byte order: @3412@
  deriving (Show, Eq)

-- |The endian of this machine, determined at run-time.

{-# NOINLINE endian #-}
endian :: Endian
endian =
  unsafePerformIO $
    allocaArray (sizeOf (undefined :: Word32)) $ \p -> do
      let val = 0x01020304 :: Word32
      poke p val
      let p' = castPtr p :: Ptr Word8
      val' <- peekArray 4 p'
      case val' of
        (0x01:0x02:0x03:0x04:[]) -> return BigEndian
        (0x04:0x03:0x02:0x01:[]) -> return LittleEndian
        (0x03:0x04:0x01:0x02:[]) -> return PDPEndian
        _                        -> error "unknown endian"
