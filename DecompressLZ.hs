module DecompressLZ(
    decompressLZ
) where

import Data.Word
import Data.Bits

data LZState = LZState{ rest    :: Int
                      , restSrc :: [Word8]
                      , revdst  :: [Word8] }

initState :: Int -> [Word8] -> LZState
initState size src = LZState size src []

srcHead :: LZState -> Word8
srcHead state = head $ restSrc state

eatSrc :: LZState -> LZState
eatSrc (LZState r s d) = LZState r (tail s) d

putDst :: Word8 -> LZState -> LZState
putDst _ (LZState 0 s d) = LZState 0 s d
putDst x (LZState r s d) = LZState (r - 1) s (x : d)

lookupDst :: Int -> LZState -> Word8
lookupDst offset (LZState _ _ d) = d !! offset

transDst :: Int -> LZState -> LZState
transDst offset x = putDst (lookupDst offset x) x

decompressLZ :: Bool -> Int -> [Word8] -> [Word8]
decompressLZ ext size src = reverse $ revdst $ decompressAll ext $ initState size src

decompressAll :: Bool -> LZState -> LZState
decompressAll ext x = if rest x == 0 then x else decompressAll ext $ decompressChunk ext x

decompressChunk :: Bool -> LZState -> LZState
decompressChunk ext x = decompressRun ext (decodeFlag $ srcHead x) (eatSrc x)

decodeFlag :: Word8 -> [Bool]
decodeFlag x = map (testBit x) [7, 6 .. 0]

decompressRun :: Bool -> [Bool] -> LZState -> LZState
decompressRun _ [] x= x
decompressRun ext (f:fs) x = decompressRun ext fs (decompressToken ext f x)

decompressToken :: Bool -> Bool -> LZState -> LZState
decompressToken ext True x = iterate (transDst offset) x' !! len
  where (x', offset, len) = parseTranspair ext x

decompressToken _ False x = eatSrc $ putDst (srcHead x) x

parseTranspair :: Bool -> LZState -> (LZState, Int, Int)
parseTranspair False x = (eatSrc $ eatSrc x, offset, len)
  where
    offset = shiftL (first .&. 0xF) 8 + second
    len = shiftR first 4 + 3
    first = fromIntegral $ srcHead x
    second = fromIntegral $ srcHead $ eatSrc x

parseTranspair True x
        | firstNibble == 0 = parseTranspair3 x
        | firstNibble == 1 = parseTranspair4 x
        | otherwise = parseTranspair2 x
        where firstNibble = shiftR (srcHead x) 4

parseTranspair2 :: LZState -> (LZState, Int, Int)
parseTranspair2 x = (eatSrc $ eatSrc x, offset, len)
  where
    offset = shiftL (first .&. 0xF) 8 + second
    len = shiftR first 4 + 1
    first = fromIntegral $ srcHead x
    second = fromIntegral $ srcHead $ eatSrc x

parseTranspair3 :: LZState -> (LZState, Int, Int)
parseTranspair3 x = (eatSrc $ eatSrc $ eatSrc x, offset, len)
  where
    offset = shiftL (second .&. 0xF) 8 + third
    len = shiftL (first .&. 0xF) 4 + shiftR second 4 + 0xF + 2
    first = fromIntegral $ srcHead x
    second = fromIntegral $ srcHead $ eatSrc x
    third = fromIntegral $ srcHead $ eatSrc $ eatSrc x

parseTranspair4 :: LZState -> (LZState, Int, Int)
parseTranspair4 x = (eatSrc $ eatSrc $ eatSrc $ eatSrc x, offset, len)
  where
    offset = shiftL (third .&. 0xF) 8 + forth
    len = shiftL (first .&. 0xF) 12 + shiftL second 4 + shiftR third 4 + 0xFF + 0xF + 3
    first = fromIntegral $ srcHead x
    second = fromIntegral $ srcHead $ eatSrc x
    third = fromIntegral $ srcHead $ eatSrc $ eatSrc x
    forth = fromIntegral $ srcHead $ eatSrc $ eatSrc $ eatSrc x
