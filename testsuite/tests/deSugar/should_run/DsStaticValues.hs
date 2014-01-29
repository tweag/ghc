{-# LANGUAGE StaticValues #-}

import GHC.Ref

main = putStr $ unlines $ map showGlobalName gNames
  where
    showGlobalName (GlobalName pkg _ m n) =
      unwords $ ("GlobalName" :) $ map show [ pkg, m, n ]
    gNames =
      [ unRef $ static g
      , unRef $ static id
      , unRef $ static (&&)
      , unRef (static method :: Ref (Char -> Int))
      , unRef $ static t_field
      ]

g :: Int -> Int
g = id

class C a where
  method :: a -> Int

instance C Char where
  method = const 0

data T a = T { t_field :: a }
