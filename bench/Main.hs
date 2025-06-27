module Main (main) where

import Lib
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "rain0"
    [ bench "trappedRain0"  $ nf trappedRain0 rain0
    , bench "trappedRain1"  $ nf trappedRain1 rain0
    , bench "trappedRain1'" $ nf trappedRain1' rain0
    , bench "trappedRain2"  $ nf trappedRain2 rain0
    , bench "trappedRain2'" $ nf trappedRain2' rain0
    , bench "trappedRain2''" $ nf trappedRain2'' rain0
    ]
  , bgroup "rain1"
    [ bench "trappedRain0"  $ nf trappedRain0 rain1
    , bench "trappedRain1"  $ nf trappedRain1 rain1
    , bench "trappedRain1'" $ nf trappedRain1' rain1
    , bench "trappedRain2"  $ nf trappedRain2 rain1
    , bench "trappedRain2'" $ nf trappedRain2' rain1
    , bench "trappedRain2''" $ nf trappedRain2'' rain1
    ]
  , bgroup "rain2"
    [ bench "trappedRain0"  $ nf trappedRain0 rain2
    , bench "trappedRain1"  $ nf trappedRain1 rain2
    , bench "trappedRain1'" $ nf trappedRain1' rain2
    , bench "trappedRain2"  $ nf trappedRain2 rain2
    , bench "trappedRain2'" $ nf trappedRain2' rain2
    , bench "trappedRain2''" $ nf trappedRain2'' rain2
    ]
  , bgroup "rain3 (large)"
    [ bench "trappedRain0"  $ nf trappedRain0 rain3
    , bench "trappedRain1"  $ nf trappedRain1 rain3
    , bench "trappedRain1'" $ nf trappedRain1' rain3
    , bench "trappedRain2"  $ nf trappedRain2 rain3
    , bench "trappedRain2'" $ nf trappedRain2' rain3
    , bench "trappedRain2''" $ nf trappedRain2'' rain3
    ]
  ]
