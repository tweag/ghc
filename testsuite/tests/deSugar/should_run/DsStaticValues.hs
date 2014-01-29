{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers       #-}

import Data.Typeable
import GHC.StaticPtr

main = putStr $ unlines $ map show gNames
  where
    gNames =
      [ -- unStaticPtr $ static g
        unStaticPtr $ (static id :: StaticPtr (Int -> Int))
        -- , unStaticPtr $ static (&&)
      , unStaticPtr $ (static t_field :: StaticPtr (T Int -> Int))
      ]

g :: Int -> Int
g = id

data T a = T { t_field :: a }
  deriving Typeable
