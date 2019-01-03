{-# LANGUAGE LinearTypes #-}
module LinearConstructors where

data T a b = MkT a b

f1 :: a ->. b ->. T a b
f1 = MkT

f2 :: a ->. b -> T a b
f2 = MkT

f3 :: a -> b ->. T a b
f3 = MkT

f4 :: a -> b -> T a b
f4 = MkT
