{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers       #-}

import Data.Typeable
import GHC.StaticPtr

main = putStr $ unlines $ map show names
  where
    names =
      [ -- unStaticPtr $ static g
        staticName $ (static id :: StaticPtr (Int -> Int))
        -- , unStaticPtr $ static (&&)
      , staticName $ (static t_field :: StaticPtr (T Int -> Int))
      ]

g :: Int -> Int
g = id

data T a = T { t_field :: a }
  deriving Typeable
