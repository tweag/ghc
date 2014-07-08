{-# LANGUAGE StaticValues   #-}
{-# LANGUAGE ExplicitForAll #-}

module StaticValuesFail01 where

import GHC.Ref

f0 :: Ref Int
f0 = static g

g :: Int -> Int
g = id

f1 = static (undefined :: (forall a . a -> a) -> b)

f2 = static (>>=)
