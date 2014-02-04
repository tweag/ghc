{-# LANGUAGE StaticValues #-}

module StaticValues01 where

import GHC.StaticRef

f0 :: StaticRef (Int -> Int)
f0 = static g

f1 :: StaticRef (Bool -> Bool -> Bool)
f1 = static (&&)

g :: Int -> Int
g = id

