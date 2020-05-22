{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vector where

import Monad.Graded
import Prelude hiding ((>>=), return)

data Nat = Zero | Succ Nat

data Vector n a where
  VNil :: Vector Zero a
  VCons :: a -> Vector n a -> Vector (Succ n) a

instance Functor (Vector n) where
  fmap f = \case
    VNil -> VNil
    VCons a v -> VCons (f a) (fmap f v)

vAppend :: Vector m a -> Vector n a -> Vector (Add m n) a
vAppend VNil v = v
vAppend (VCons a u) v = VCons a (vAppend u v)

toList :: Vector n a -> [a]
toList = \case
  VNil -> []
  VCons a v -> a : toList v

fail :: String -> Vector n a
fail = error

class VRepeat n where
  vRepeat :: a -> Vector n a
instance VRepeat Zero where
  vRepeat _ = VNil
instance VRepeat n => VRepeat (Succ n) where
  vRepeat a = VCons a (vRepeat a)

type family Add m n :: Nat where
  Add Zero n = n
  Add (Succ m) n = Succ (Add m n)

type family Times m n :: Nat where
  Times Zero n = Zero
  Times (Succ m) n = Add n (Times m n)

instance GradedMonad Vector where
  type Unit Vector = Succ Zero
  type Plus Vector i j = Times i j
  type Inv  Vector i j = ()
  v >>= f = case v of
    VNil -> VNil
    VCons a v -> vAppend (f a) (v >>= f)
  return a = VCons a VNil
