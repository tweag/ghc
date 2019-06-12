{-# LANGUAGE GADTSyntax, DataKinds, LinearTypes, KindSignatures, ExplicitForAll #-}
module MultConstructor where

import GHC.Types

data T p a where
  MkT :: a -->.(p) T p a

g :: forall (b :: Type). T 'Omega b ->. (b,b)
g (MkT x) = (x,x)
