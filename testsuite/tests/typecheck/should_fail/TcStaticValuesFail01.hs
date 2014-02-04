{-# LANGUAGE StaticValues #-}

module StaticValuesFail01 where

import GHC.StaticRef

f0 :: StaticRef Int
f0 = static g

g :: Int -> Int
g = id

