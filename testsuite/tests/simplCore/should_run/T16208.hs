{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import GHC.Exts
import Unsafe.Coerce

newtype Age a b where
  Age :: forall b a. Int -> Age a b

data T a = MkT a | Nil

{-# NOINLINE mapT #-}
mapT :: (Int -> Age Bool Char) -> Int
mapT _ = 0

{-# RULES "mapT/c" [1] mapT coerce = 1 #-}

main = print (mapT Age)
