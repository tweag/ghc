{-# LANGUAGE StaticValues #-}
{-# LANGUAGE ImpredicativeTypes #-}

import GHC.StaticRef

main = putStr $ unlines $ map showGlobalName gNames
  where
    globalNameStaticRef (StaticRef gn) = gn
    showGlobalName (GlobalName pkg _ m n) =
      unwords $ ("GlobalName" :) $ map show [ pkg, m, n ]
    gNames =
      [ globalNameStaticRef $ static g
      , globalNameStaticRef $ static id
      , globalNameStaticRef $ static (&&)
      , globalNameStaticRef $ static method
      , globalNameStaticRef $ static t_field
      ]

g :: Int -> Int
g = id

class C a where
  method :: a -> Int

data T a = T { t_field :: a }
