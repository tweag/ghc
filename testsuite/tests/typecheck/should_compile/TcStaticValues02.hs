{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers     #-}

module StaticValues02 where

import GHC.StaticPtr
import Data.Typeable

f2 :: Typeable a => StaticPtr (a -> a)
f2 = static id

f4 :: Typeable a => StaticPtr (T a -> a)
f4 = static t_field

g :: Int -> Int
g = id

data T a = T { t_field :: a }
  deriving Typeable
