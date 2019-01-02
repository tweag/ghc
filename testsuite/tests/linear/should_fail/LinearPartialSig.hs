{-# LANGUAGE LinearTypes #-}
module LinearPartialSig where

-- We should suggest that _ :: Multiplicity
f :: a -->.(_) a
f x = x

g :: a -->.(_) a
g x = const x x
