{-# LANGUAGE StaticValues #-}

module StaticValuesFail01 where

import GHC.Ref

f0 :: Ref Int
f0 = static g

g :: Int -> Int
g = id

