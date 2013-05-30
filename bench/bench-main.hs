module Main where

import ClassyPrelude
import Criterion.Main

main :: IO ()
main = do
    defaultMain [
        bgroup "fib" [ bench "10" $ whnf fib 10
                     , bench "35" $ whnf fib 35
                     , bench "37" $ whnf fib 37
                     ]
       ]


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
