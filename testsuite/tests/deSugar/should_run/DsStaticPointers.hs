{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers       #-}

import Data.Typeable
import GHC.StaticPtr

main = putStr $ unlines $ map show names
  where
    names =
      [ staticName $ static g
      , staticName $ (static id :: StaticPtr (Int -> Int))
      , staticName $ (p0 :: StaticPtr (Int -> Int))
      , staticName $ (static method :: StaticPtr (Char -> Int))
      , staticName $ (static t_field :: StaticPtr (T Int -> Int))
      ]

g :: Int -> Int
g = id

p0 :: Typeable a => StaticPtr (a -> a)
p0 = static (\x -> x)

data T a = T { t_field :: a }
  deriving Typeable

class C1 a where
  method :: a -> Int

instance C1 Char where
  method = const 0
