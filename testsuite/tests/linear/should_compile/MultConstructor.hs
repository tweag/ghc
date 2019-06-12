{-# LANGUAGE GADTSyntax, DataKinds, LinearTypes, KindSignatures, ExplicitForAll #-}
module MultConstructor where

import GHC.Types

data T (p :: Multiplicity) a where
  MkT :: a -->.(p) T p a

{-
data T :: Multiplicity -> (Multiplicity -> *) -> * where
  MkT :: f p -->.(p) T p f
-}

{-
g :: T Omega b ->. (b,b)
g (MkT x) = (x,x)
-}
