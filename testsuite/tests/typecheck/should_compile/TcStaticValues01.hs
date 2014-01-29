{-# LANGUAGE StaticValues #-}

module StaticValues01 where

import GHC.Ref

f0 :: Ref (Int -> Int)
f0 = static g

f1 :: Ref (Bool -> Bool -> Bool)
f1 = static (&&)

f2 :: Ref (Bool -> Bool -> Bool)
f2 = static ((&&) . id)

g :: Int -> Int
g = id

