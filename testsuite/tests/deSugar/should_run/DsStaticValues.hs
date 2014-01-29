{-# LANGUAGE StaticValues #-}

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
      , globalNameRef $ static (method :: Char -> Int)
      , globalNameRef $ static t_field
      ]

g :: Int -> Int
g = id

class C a where
  method :: a -> Int

instance C Char where
  method = const 0

data T a = T { t_field :: a }
