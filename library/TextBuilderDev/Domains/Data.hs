module TextBuilderDev.Domains.Data where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text.Array as TextArray
import qualified GHC.List
import TextBuilderDev.Core
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Other
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Prelude hiding (intercalate, length, null)

-- | Hexadecimal readable representation of binary data.
--
-- >>> hexData "Hello"
-- "4865 6c6c 6f"
{-# INLINE hexData #-}
hexData :: ByteString -> TextBuilder
hexData =
  intercalate " "
    . fmap mconcat
    . Split.chunksOf 2
    . fmap byte
    . ByteString.unpack
  where
    byte =
      padFromLeft 2 '0' . unsignedHexadecimal

-- | Bits of a statically sized value.
-- If it's an integer, the sign is reflected in the bits.
--
-- >>> bits @Int 0
-- "0"
--
-- >>> bits @Int 4
-- "100"
--
-- >>> bits @Int8 (-4)
-- "11111100"
{-# INLINE bits #-}
bits :: (FiniteBits a) => a -> TextBuilder
bits val =
  let size = max 1 (finiteBitSize val - countLeadingZeros val)
   in TextBuilder size \array arrayStartIndex ->
        let go val arrayIndex =
              if arrayIndex >= arrayStartIndex
                then do
                  TextArray.unsafeWrite array arrayIndex
                    $ if testBit val 0 then 49 else 48
                  go (unsafeShiftR val 1) (pred arrayIndex)
                else return indexAfter
            indexAfter =
              arrayStartIndex + size
         in go val (pred indexAfter)

-- | Bits of a statically sized value padded from the left according to the size.
-- If it's an integer, the sign is reflected in the bits.
--
-- >>> paddedBits @Int8 0
-- "00000000"
--
-- >>> paddedBits @Int8 4
-- "00000100"
--
-- >>> paddedBits @Int16 4
-- "0000000000000100"
--
-- >>> paddedBits @Int16 (-4)
-- "1111111111111100"
{-# INLINE paddedBits #-}
paddedBits :: (FiniteBits a) => a -> TextBuilder
paddedBits val =
  let size = finiteBitSize val
   in TextBuilder size \array arrayStartIndex ->
        let go val arrayIndex =
              if arrayIndex >= arrayStartIndex
                then do
                  TextArray.unsafeWrite array arrayIndex $ if testBit val 0 then 49 else 48
                  go (unsafeShiftR val 1) (pred arrayIndex)
                else return indexAfter
            indexAfter =
              arrayStartIndex + size
         in go val (pred indexAfter)

-- | Bits of a statically sized value padded from the left according to the size.
-- If it's an integer, the sign is reflected in the bits.
--
-- >>> paddedBits2 @Int8 0
-- "00000000"
--
-- >>> paddedBits2 @Int8 4
-- "00000100"
--
-- >>> paddedBits2 @Int16 4
-- "0000000000000100"
--
-- >>> paddedBits2 @Int16 (-4)
-- "1111111111111100"
{-# INLINE paddedBits2 #-}
paddedBits2 :: (FiniteBits a) => a -> TextBuilder
paddedBits2 val =
  unsafeSeptets size list
  where
    size = finiteBitSize val
    list =
      GHC.List.build \cons nil ->
        let go index =
              if index >= 0
                then
                  let codepoint = if testBit val index then 49 else 48
                   in cons codepoint (go (pred index))
                else nil
         in go (pred size)

-- | Bits of a statically sized value padded from the left according to the size.
-- If it's an integer, the sign is reflected in the bits.
--
-- >>> paddedBits3 @Int8 0
-- "00000000"
--
-- >>> paddedBits3 @Int8 4
-- "00000100"
--
-- >>> paddedBits3 @Int16 4
-- "0000000000000100"
--
-- >>> paddedBits3 @Int16 (-4)
-- "1111111111111100"
{-# INLINE paddedBits3 #-}
paddedBits3 :: (FiniteBits a) => a -> TextBuilder
paddedBits3 val =
  unsafeReverseSeptets size list
  where
    size = finiteBitSize val
    list =
      GHC.List.build \cons nil ->
        let go index =
              if index < size
                then
                  let codepoint = if testBit val index then 49 else 48
                   in cons codepoint (go (succ index))
                else nil
         in go 0
