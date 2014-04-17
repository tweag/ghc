{-# LANGUAGE StaticValues #-}
{-# LANGUAGE ImpredicativeTypes #-}

module StaticValues02 where

import GHC.Ref

f2 :: Ref (a -> a)
f2 = static id

f3 :: Ref (C a => a -> Int)
f3 = static method

f4 :: Ref (T a -> a)
f4 = static t_field

f5 :: Ref (a -> a)
f5 = static (id . id)

g :: Int -> Int
g = id

class C a where
  method :: a -> Int

data T a = T { t_field :: a }
