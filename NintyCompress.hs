module NintyCompress(
    decompress
) where

import Data.Word
import Data.Bits
import DecompressLZ

decompress :: [Word8] -> [Word8]
decompress src = uncurry (decompressWithTypeAndSize (head src))  $ splitSizeAndData src

splitSizeAndData :: [Word8] -> (Int, [Word8])
splitSizeAndData src = (fromOctets $ take 3 $ tail src, drop 4 src)

fromOctets :: [Word8] -> Int
fromOctets = foldr accum 0
  where
    accum o a = (a `shiftL` 8) .|. fromIntegral o

-- TODO support other type --
decompressWithTypeAndSize :: Word8 -> Int -> [Word8] -> [Word8]
decompressWithTypeAndSize 0x10 = decompressLZ False
decompressWithTypeAndSize 0x11 = decompressLZ True
decompressWithTypeAndSize _ = error "Unknown compression type"
