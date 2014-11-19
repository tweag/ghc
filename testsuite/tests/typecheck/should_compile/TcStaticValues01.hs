{-# LANGUAGE StaticPointers #-}

module StaticValues01 where

import GHC.StaticPtr

f0 :: StaticPtr (Int -> Int)
f0 = static g

f1 :: StaticPtr (Bool -> Bool -> Bool)
f1 = static (&&)

g :: Int -> Int
g = id
