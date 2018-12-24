{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS -Wno-missing-methods #-}

-- | This module defines the semi-ring (aka Mult) of multiplicities, and associated
-- functions. Multiplicities annotate arrow types to indicate the linearity of the
-- arrow (in the sense of linear types).
module Multiplicity
  ( Mult
  , pattern One
  , pattern Omega
  , pattern MultAdd
  , pattern MultMul
  , pattern MultThing
  , sup
  , Scaled(..)
  , unrestricted
  , linear
  , tymult
  , irrelevantMult
  , mkScaled
  , scaledSet
  , scaleScaled
  , IsSubmult(..)
  , submult
  , traverseMult
  , multThingList
  , mapMult
  , fromMult
  , toMult ) where

import GhcPrelude

import Data.Data
import Outputable
import {-# SOURCE #-} TyCoRep (Type)
import {-# SOURCE #-} TysWiredIn ( oneDataConTy, omegaDataConTy )
import {-# SOURCE #-} Type( eqType )

{-
Note [Adding new multiplicities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To add a new multiplicity, you need to:
* Add the new value to the Mult type
* Define new cases in fromMult/toMult functions,
  possibly defining new syntax for the multiplicity
* Update cases in MultAdd, MultMul, sup, submult, tcSubmult
* Check supUE function that computes sup of a multiplicity
  and Zero
-}

--
-- * Core properties of multiplicities
--

data Mult
  = One_
  | Omega_
  | MultAdd_ Mult Mult
  | MultMul_ Mult Mult
  | MultThing_ Type
  deriving (Data)

-- We may enforce more invariants in Mult. For instance, we can
-- enforce that it is in the form of a sum of products, and even that the
-- sumands and factors are ordered somehow, to have more equalities.

fromMult :: Mult -> Type
fromMult One = oneDataConTy
fromMult Omega = omegaDataConTy
fromMult (MultThing ty) = ty
fromMult _ =
  pprPanic "Type.fromMult" (text "Full support for multiplicity polymorphism is not implemented yet")

toMult :: Type -> Mult
toMult ty
  | oneDataConTy `eqType` ty = One
  | omegaDataConTy `eqType` ty = Omega
  | otherwise = MultThing_ ty

-- Note that pattern synonyms for One and Omega are not necessary: we could just
-- export them as constructors. They are defined as pattern synonym for
-- symmetry. Following the principle of least surprise.

pattern One :: Mult
pattern One = One_

pattern Omega :: Mult
pattern Omega = Omega_

pattern MultMul :: Mult -> Mult -> Mult
pattern MultMul p q <- MultMul_ p q where
  One `MultMul` p = p
  p `MultMul` One = p
  Omega `MultMul` _ = Omega
  _ `MultMul` Omega = Omega
  p `MultMul` q = MultMul_ p q

pattern MultAdd :: Mult -> Mult -> Mult
pattern MultAdd p q <- MultAdd_ p q where
  One `MultAdd` One = Omega
  Omega `MultAdd` _ = Omega
  _ `MultAdd` Omega = Omega
  p `MultAdd` q = MultAdd_ p q

pattern MultThing :: Type -> Mult
pattern MultThing a <- MultThing_ a where
  MultThing a = toMult a

{-# COMPLETE One, Omega, MultMul, MultAdd, MultThing #-}

instance Outputable Mult where
  ppr One = text "1"
  ppr Omega = text "Omega"
  ppr (MultAdd m1 m2) = parens (ppr m1 <+> text "+" <+> ppr m2)
  ppr (MultMul m1 m2) = parens (ppr m1 <+> text "*" <+> ppr m2)
  ppr (MultThing t) = ppr t

-- | @sup w1 w2@ returns the smallest multiplicity larger than or equal to both @w1@
-- and @w2@.
sup :: Mult -> Mult -> Mult
sup One   One   = One
sup Omega Omega = Omega
sup _     _     = Omega
-- Note: If you are changing this logic, check 'supUE' in UsageEnv as well.
--
-- I assume that `sup` is incomplete in presence of multiplicity
-- polymorphism. Maybe we need a syntactic join operation on multiplicities.


--
-- * Utilities
--

-- | A shorthand for data with an attached 'Mult' element (the multiplicity).
data Scaled a = Scaled {scaledMult :: Mult, scaledThing :: a}
  deriving (Functor,Foldable,Traversable,Data)

unrestricted, linear, tymult :: a -> Scaled a
unrestricted = Scaled Omega
linear = Scaled One

-- Used for type arguments in core
tymult = Scaled Omega

irrelevantMult :: Scaled a -> a
irrelevantMult = scaledThing

mkScaled :: Mult -> a -> Scaled a
mkScaled = Scaled

instance (Outputable a) => Outputable (Scaled a) where
   ppr (Scaled _cnt t) = ppr t
     -- Do not print the multiplicity here because it tends to be too verbose

scaledSet :: Scaled a -> b -> Scaled b
scaledSet x b = fmap (\_->b) x

scaleScaled :: Mult -> Scaled a -> Scaled a
scaleScaled w x =
  x { scaledMult = w `MultMul` scaledMult x }

--
-- * Multiplicity ordering
--

data IsSubmult = Submult     -- Definitely a submult
               | NotSubmult  -- Definitely not a submult
               | Unknown     -- Could be a submult, need to ask the typechecker
               deriving (Show, Eq, Ord)

instance Outputable IsSubmult where
  ppr = text . show

-- | @submult w1 w2@ check whether a value of multiplicity @w1@ is allowed where a
-- value of multiplicity @w2@ is expected. This is a partial order.

submult :: Mult -> Mult -> IsSubmult
submult _     Omega = Submult
submult Omega One   = NotSubmult
submult One   One   = Submult
-- The 1 <= p rule
submult One   _     = Submult
--    submult (MultThing t) (MultThing t') = Unknown
submult _     _     = Unknown

traverseMult :: Applicative f => (Type -> f Type) -> Mult -> f Mult
traverseMult _ One = pure One
traverseMult _ Omega = pure Omega
traverseMult f (MultThing t) = MultThing <$> f t
traverseMult f (MultAdd x y) = MultAdd <$> traverseMult f x <*> traverseMult f y
traverseMult f (MultMul x y) = MultMul <$> traverseMult f x <*> traverseMult f y

multThingList :: (Type -> a) -> Mult -> [a]
multThingList f = go []
  where go acc One = acc
        go acc Omega = acc
        go acc (MultThing t) = f t : acc
        go acc (MultAdd x y) = go (go acc y) x
        go acc (MultMul x y) = go (go acc y) x

mapMult :: (Type -> Type) -> Mult -> Mult
mapMult _ One = One
mapMult _ Omega = Omega
mapMult f (MultThing t) = MultThing (f t)
mapMult f (MultAdd x y) = MultAdd (mapMult f x) (mapMult f y)
mapMult f (MultMul x y) = MultMul (mapMult f x) (mapMult f y)
