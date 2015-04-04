module BabyMonitor.UIdDIY where

import BabyMonitor.UId
import qualified Data.Bitstream as BS
import Data.Bitstream (Bitstream, toBits)
import Data.Binary
import ClassyPrelude
import Data.List.Split

----------------------------------------------------------------

-- Well my idea of user friendliness maximized ids is not that new, it
-- is exactly in the same way as I made it up standardized in RFC
-- 3548, and there is more than one haskell library available already
-- implementing it. One of them is used in UId. I am keeping the DIY
-- variant for now, for testing.

-- Convert an uid to a string suitable to be displayed to a user
toUserStringDIY :: Uid -> String
toUserStringDIY (Uid uid) = intercalate "-" . chunksOf 4 . rawString $ uid
    where
      rawString :: ByteString -> String
      rawString = map show5BitBlock . split5Bits . BS.fromByteString . reverseTimeLow

split5Bits :: Bitstream BS.Right -> [Word8]
split5Bits = map toBits . blocks5Bits 


blocks5Bits :: Bitstream BS.Right -> [Bitstream BS.Right]
blocks5Bits bs = if bs == BS.empty then []
                 else BS.take blockSize bs : blocks5Bits (BS.drop blockSize bs)
    where blockSize = 5 :: Int

show5BitBlock :: Word8 -> Char
show5BitBlock w = if w < 32
                  then
                      if w < 26
                      then toEnum $ wi + letter0
                      else toEnum $ wi - 26 + digit0
                  else
                    error "Not a 5 bit block!"
    where
      letter0 = fromEnum 'a'
      digit0 = fromEnum '2' -- We start with 2 to avoid confusions like 1 looking similar to l in some fonts.
      wi = fromIntegral w
