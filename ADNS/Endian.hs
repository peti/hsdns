{- |
   Module      :  ADNS.Endian
   Copyright   :  (c) 2008 Peter Simons
   License     :  LGPL

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   Determine the machine's endian.
-}

module ADNS.Endian ( Endian(..), endian, readWord32, readWord16 ) where

import Foreign
import System.IO.Unsafe

-- |Signify the system's native byte order according to
-- significance of bytes from low addresses to high addresses.

data Endian
  = LittleEndian                -- ^ byte order: @1234@
  | BigEndian                   -- ^ byte order: @4321@
  | PDPEndian                   -- ^ byte order: @3412@
  deriving (Show, Eq)

-- |The endian of this machine, determined at run-time.

{-# NOINLINE endian #-}
endian :: Endian
endian =
  System.IO.Unsafe.unsafePerformIO $
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

-- |Parse a host-ordered 32-bit word into a network-ordered tuple
-- of 8-bit words.

readWord32 :: Word32 -> (Word8, Word8, Word8, Word)
readWord32 n =
  let (b1,n1) = (n  .&. 255, n  `shiftR` 8)
      (b2,n2) = (n1 .&. 255, n1 `shiftR` 8)
      (b3,n3) = (n2 .&. 255, n2 `shiftR` 8)
      b4      = n3 .&. 255
  in
  case endian of
    BigEndian    -> (fromIntegral b4, fromIntegral b3, fromIntegral b2, fromIntegral b1)
    LittleEndian -> (fromIntegral b1, fromIntegral b2, fromIntegral b3, fromIntegral b4)
    PDPEndian    -> (fromIntegral b2, fromIntegral b1, fromIntegral b4, fromIntegral b3)

-- |Parse a host-ordered 16-bit word into a network-ordered tuple of
-- 8-bit words.

readWord16 :: Word16 -> (Word8, Word8)
readWord16 n =
  let (b1,n1) = (n  .&. 255, n  `shiftR` 8)
      b2      = n1 .&. 255
  in
  case endian of
    BigEndian    -> (fromIntegral b2, fromIntegral b1)
    LittleEndian -> (fromIntegral b1, fromIntegral b2)
    PDPEndian    -> (fromIntegral b2, fromIntegral b1)
