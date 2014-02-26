{-# LANGUAGE StaticValues #-}
{-# LANGUAGE ImpredicativeTypes #-}

module StaticValues02 where

import GHC.StaticRef

f2 :: StaticRef (a -> a)
f2 = static id

f3 :: StaticRef (C a => a -> Int)
f3 = static method

f4 :: StaticRef (T a -> a)
f4 = static t_field

f5 :: StaticRef (a -> a)
f5 = static (id . id)

g :: Int -> Int
g = id

class C a where
  method :: a -> Int

data T a = T { t_field :: a }
