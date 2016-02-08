{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
module Codec.Compression.LZF
    ( compress
    , decompress
    , LZFCompressed(..)
    , LZFDecompressionError(..)
    , BufferStrategy(..)
    , defaultBufferStrategy
    ) where


-------------------------------------------------------------------------------
import           Control.Exception
import Debug.Trace
import           Data.Bits
import           Data.ByteString   as BS
import           Data.Typeable
import           Foreign
import           Foreign.C
-------------------------------------------------------------------------------

--TODO: why IO? should these be unsafe?
{-# CFILES liblzf-1.6/lzf_c.c liblzf-1.6/lzf_d.c #-}
foreign import ccall unsafe "lzf_compress" _compress :: Ptr a -> CUInt -> Ptr b -> CUInt -> IO CUInt
foreign import ccall unsafe "lzf_decompress" _decompress :: Ptr a -> CUInt -> Ptr b -> CUInt -> IO CUInt

-- | Compress a block of data. The length of data is
-- not recorded. Returns the length of output or 0
-- if it is longer than the size of the output buffer.
compress' :: CString -> Int -> CString -> Int -> IO Int
compress' input ilen output olen = do
  res <- _compress input (fromIntegral ilen) output (fromIntegral olen)
  return (fromIntegral res)

-- | Decompress a block of data. Returns the length
-- of data uncompressed or 0 on error.
decompress' :: CString -> Int -> CString -> Int -> IO Int
decompress' input ilen output olen = do
  res <- _decompress input (fromIntegral ilen) output (fromIntegral olen)
  return (fromIntegral res)


newtype LZFCompressed = LZFCompressed {
      lzfCompressed :: ByteString
    } deriving (Show, Eq, Ord)


--TODO: is any of this referentially transparent enough to use unsafePerformIO?

--TODO: look carefully at buffer size requirements for compression
-- i think i need to rethink this. maybe we need to do this blockwise, otherwise we need to know exact length when decrypting
compress :: ByteString -> IO LZFCompressed
compress str
  -- No point in compressing a null string, and the length would come
  -- back as 0 (which is also the error code, thanks C).
  | BS.null str = return (LZFCompressed str)
  | otherwise = do
    BS.useAsCStringLen str $ \(inp, len) ->
      allocaBytes len $ \out -> do
        -- lzf actually recommends using a len - 1 buffer size to
        -- ensure that the final string is smaller and if its not, to
        -- store it with your own flag uncompressed. This is not a
        -- safe assumption for this library because we cannot
        -- guarantee that you'll only be decompressing your own
        -- strings. One may use this library to compress strings that
        -- some other library created, therefor the safest thing is to
        -- always compress or decompress with LZF, even if the output
        -- is larger. This also means we can't do anything clever like
        -- prepend a length to the compressed string.
        --
        -- They do mention that it will be less than 104% of original
        -- size. We're going to treat this as a reliable constant and always allocate 105% of original length
        -- let bufLen = ceiling (bufferFactor * ((len + 2) % 1))
         --TODO: why does this magic number just work...
        let bufLen = (lzfMaxCompressedSize len) + 3
        resLen <- compress' inp len out bufLen
        rawRes <- if resLen == 0
                    then traceShow (BS.length str, len, bufLen, str) $ error "Impossible compression buffer size error"
                    else do BS.packCStringLen (out, (fromIntegral resLen))
        return (LZFCompressed rawRes)
  where
    -- lifted from the LZF_MAX_COMPRESSED_SIZE in 1.6
    lzfMaxCompressedSize n = (((n) * 33) `shiftR` 5 ) + 1


data BufferStrategy = KnownUncompressedSize !Int
                    -- ^ Often when using lzf compression you encode
                    -- the uncompressed string size before the
                    -- compressed data. Redis does this, for instance. If this option is chosen and it is incorrect,
                    | GrowBufferBytes !Int
                    -- ^ Try this buffer size and keep growing it by that size until the output fits.


-- | Grow buffer by 1024 bytes until the output fits
defaultBufferStrategy :: BufferStrategy
defaultBufferStrategy = GrowBufferBytes 1024


data LZFDecompressionError = DecompressionBufferTooSmall
                           -- ^ The given decompressed size was too small
                           | InvalidLZF
                           -- ^ The given string was not valid LZF
                           deriving (Show, Eq, Typeable)


instance Exception LZFDecompressionError


decompress :: BufferStrategy -> LZFCompressed -> IO (Either LZFDecompressionError ByteString)
decompress strat (LZFCompressed str)
  | BS.null str = return (Right str)
  | otherwise = BS.useAsCStringLen str $ \(inStr, inStrLen) ->
      go inStr inStrLen startingBufSize
  where
    startingBufSize = case strat of
                        GrowBufferBytes n       -> n
                        KnownUncompressedSize n -> n
    go inStr inStrLen bufSize =
      allocaBytes bufSize $ \outStr -> do
        res <- decompress' inStr inStrLen outStr bufSize
        case res of
          0 -> do
            en <- getErrno
            if en == e2BIG
               then case strat of
                 GrowBufferBytes bufGrow -> go inStr inStrLen (bufSize + bufGrow)
                 _ -> return (Left DecompressionBufferTooSmall)
               else return (Left InvalidLZF)
          actualLen -> Right <$> BS.packCStringLen (outStr, actualLen)
