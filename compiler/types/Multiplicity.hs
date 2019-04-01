{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{-|
This module defines the semi-ring of multiplicities, and associated functions.
Multiplicities annotate arrow types to indicate the linearity of the
arrow (in the sense of linear types).

Mult is a type synonym for Type, used only when its kind is Multiplicity.
To simplify dealing with multiplicities, smart constructors such as
mkMultAdd perform simplifications such as Omega + x = x on the fly.
Pattern synonyms such as MultAdd can be used to analyze particular Mults.
-}
module Multiplicity
  ( Mult
  , pattern One
  , pattern Omega
  , pattern MultAdd
  , pattern MultMul
  , mkMultAdd
  , mkMultMul
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
  , submult) where

import GhcPrelude

import Data.Data
import Outputable
import {-# SOURCE #-} TyCoRep (Type)
import {-# SOURCE #-} TysWiredIn ( oneDataConTy, omegaDataConTy, multAddTyCon, multMulTyCon )
import {-# SOURCE #-} Type( eqType, splitTyConApp_maybe, mkTyConApp )
import PrelNames (multAddTyConKey, multMulTyConKey)
import Unique (hasKey)

{-
Note [Adding new multiplicities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To add a new multiplicity, you need to:
* Add the new type with Multiplicity kind
* Update cases in MultAdd, MultMul, sup, submult, tcSubmult
* Check supUE function that computes sup of a multiplicity
  and Zero
-}

--
-- * Core properties of multiplicities
--

type Mult = Type

-- We may enforce more invariants in Mult. For instance, we can
-- enforce that it is in the form of a sum of products, and even that the
-- sumands and factors are ordered somehow, to have more equalities.

pattern One :: Mult
pattern One <- (eqType oneDataConTy -> True)
  where One = oneDataConTy

pattern Omega :: Mult
pattern Omega <- (eqType omegaDataConTy -> True)
  where Omega = omegaDataConTy

isMultAdd :: Mult -> Maybe (Mult, Mult)
isMultAdd ty | Just (tc, [x, y]) <- splitTyConApp_maybe ty
             , tc `hasKey` multAddTyConKey = Just (x, y)
             | otherwise = Nothing

isMultMul :: Mult -> Maybe (Mult, Mult)
isMultMul ty | Just (tc, [x, y]) <- splitTyConApp_maybe ty
             , tc `hasKey` multMulTyConKey = Just (x, y)
             | otherwise = Nothing

pattern MultAdd :: Mult -> Mult -> Mult
pattern MultAdd p q <- (isMultAdd -> Just (p,q))

pattern MultMul :: Mult -> Mult -> Mult
pattern MultMul p q <- (isMultMul -> Just (p,q))

mkMultAdd :: Mult -> Mult -> Mult
mkMultAdd One One = Omega
mkMultAdd Omega _ = Omega
mkMultAdd _ Omega = Omega
mkMultAdd p q     = mkTyConApp multAddTyCon [p, q]

mkMultMul :: Mult -> Mult -> Mult
mkMultMul One p = p
mkMultMul p One = p
mkMultMul Omega _ = Omega
mkMultMul _ Omega = Omega
mkMultMul p q | p `eqType` q = p
              | otherwise = mkTyConApp multMulTyCon [p, q]

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
   ppr (Scaled _cnt t) = ppr t <> text "[" <> ppr _cnt <> text "]"
     -- Do not print the multiplicity here because it tends to be too verbose

scaledSet :: Scaled a -> b -> Scaled b
scaledSet x b = fmap (\_->b) x

scaleScaled :: Mult -> Scaled a -> Scaled a
scaleScaled w x =
  x { scaledMult = w `mkMultMul` scaledMult x }

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
submult _     _     = Unknown
