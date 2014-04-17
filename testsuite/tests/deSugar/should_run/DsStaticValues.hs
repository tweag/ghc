{-# LANGUAGE StaticValues #-}
{-# LANGUAGE ImpredicativeTypes #-}

import GHC.Ref

main = putStr $ unlines $ map showGlobalName gNames
  where
    globalNameRef (Ref gn) = gn
    showGlobalName (GlobalName pkg _ m n) =
      unwords $ ("GlobalName" :) $ map show [ pkg, m, n ]
    gNames =
      [ globalNameRef $ static g
      , globalNameRef $ static id
      , globalNameRef $ static (&&)
      , globalNameRef $ static method
      , globalNameRef $ static t_field
      , globalNameRef $ static (id . show)
      , globalNameRef $ static (id . (+))
      ]

g :: Int -> Int
g = id

class C a where
  method :: a -> Int

data T a = T { t_field :: a }
