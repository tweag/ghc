{-# LANGUAGE StaticValues #-}

module StaticValues02 where

import GHC.Ref

f2 :: Ref (a -> a)
f2 = static id

f3 :: Ref (Char -> Int)
f3 = static method

f4 :: Ref (T a -> a)
f4 = static t_field

f5 :: Ref (a -> a)
f5 = static (id . id)

f6 :: Ref (a -> IO a)
f6 = static return

f7 :: Ref (a -> IO a)
f7 = static (\x -> getLine >> return x)

g :: Int -> Int
g = id

class C a where
  method :: a -> Int

instance C Char where
  method = const 0

data T a = T { t_field :: a }
