module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import qualified Data.ByteString           as BS
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic
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
    testProperty "compress/decompress cycle with growing buffer" $ monadicIO $ do
      bs <- pick arbitrary
      Positive bufSize <- pick arbitrary
      res <- run (decompress (GrowBufferBytes bufSize) =<< compress bs)
      assert (res == Right bs)
  , testProperty "compress/decompress cycle with correct known buffer size" $ monadicIO $ do
      bs <- pick arbitrary
      let exactBufSize = BS.length bs
      res <- run (decompress (KnownUncompressedSize exactBufSize) =<< compress bs)
      assert (res == Right bs)
  ]
