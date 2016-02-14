module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import qualified Data.ByteString           as BS
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Codec.Compression.LZF
-------------------------------------------------------------------------------




main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "lzf-bytestring"
  [
    testProperty "compress/decompress cycle with growing buffer" $ \bs (Positive bufSize) -> 
      let res = decompress (GrowBufferBytes bufSize) (compress bs)
      in res === Right bs
  , testProperty "compress/decompress cycle with correct known buffer size" $ \bs ->
      let exactBufSize = BS.length bs
          res = decompress (KnownUncompressedSize exactBufSize) (compress bs)
      in res === Right bs
  ]
