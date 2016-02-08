{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
import           Data.ByteString       as BS
-------------------------------------------------------------------------------
import           Codec.Compression.LZF
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain
  [ compressBenchmark
  , decompressBenchmark
  ]


-------------------------------------------------------------------------------
compressBenchmark :: Benchmark
compressBenchmark = bench "compress" (whnfIO (compress example))


-------------------------------------------------------------------------------
decompressBenchmark :: Benchmark
decompressBenchmark = env (lzfCompressed <$> compress example) $ \ ~(compressed) -> do
  bench "decompress" (whnfIO (decompress (KnownUncompressedSize exampleLength) (LZFCompressed compressed)))

-------------------------------------------------------------------------------
example :: ByteString
example = "The quick brown fox jumps over the lazy dog"


-------------------------------------------------------------------------------
exampleLength :: Int
exampleLength = BS.length example
