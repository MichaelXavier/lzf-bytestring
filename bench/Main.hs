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
compressBenchmark = bench "compress" (whnf compress example)


-------------------------------------------------------------------------------
decompressBenchmark :: Benchmark
decompressBenchmark =
  bench "decompress" (whnf (decompress (KnownUncompressedSize exampleLength)) compressedExample)

-------------------------------------------------------------------------------
example :: ByteString
example = "The quick brown fox jumps over the lazy dog"


-------------------------------------------------------------------------------
compressedExample :: LZFCompressed
compressedExample = compress example

-------------------------------------------------------------------------------
exampleLength :: Int
exampleLength = BS.length example
