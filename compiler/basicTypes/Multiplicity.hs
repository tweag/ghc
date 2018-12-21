{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS -Wno-missing-methods #-}

-- | This module defines the semi-ring (aka Mult) of multiplicities, and associated
-- functions. Multiplicities annotate arrow types to indicate the linearity of the
-- arrow (in the sense of linear types).
module Multiplicity
  ( GMult
  , pattern One
  , pattern Omega
  , pattern MultAdd
  , pattern MultMul
  , pattern MultThing
  , Multable(..)
  , unsafeMultThing
  , sup
  , GScaled(..)
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
  , mapMult ) where

import GhcPrelude

import Data.Data
import Outputable

{-
Note [Adding new multiplicities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To add a new multiplicity, you need to:
* Add the new value to the GMult type
* Define new cases in fromMult/toMult functions in TyCoRep,
  possibly defining new syntax for the multiplicity
* Update cases in MultAdd, MultMul, sup, submult, tcSubmult
* Check supUE function that computes sup of a multiplicity
  and Zero
-}

--
-- * Core properties of multiplicities
--

data GMult a
  = One_
  | Omega_
  | MultAdd_ (GMult a) (GMult a)
  | MultMul_ (GMult a) (GMult a)
  | MultThing_ a
  deriving (Data)

-- | The 'Multable' class describes the requirements that a type needs to be a
-- good citizen as an argument of 'GMult'
class Multable a where
  -- | A way to relect multiplicities into @a@
  fromMult :: GMult a -> a

  -- | A way to reify multiplicities from a value of @a@. @fromMult . toMult@
  -- should be the identity for a suitable equality. It is also expected that
  -- @toMult t@ reifies @t@ as much as possible. A more formal requirement is
  -- that @m@ is a subtree of @toMult . fromMult m@.
  toMult :: a -> GMult a

-- Note that pattern synonyms for One and Omega are not necessary: we could just
-- export them as constructors. They are defined as pattern synonym for
-- symmetry. Following the principle of least surprise.

-- We may enforce more invariants in the type of GMult. For instance, we can
-- enforce that it is in the form of a sum of products, and even that the
-- sumands and factors are ordered somehow, to have more equalities.

pattern One :: GMult a
pattern One = One_

pattern Omega :: GMult a
pattern Omega = Omega_

pattern MultMul :: GMult a -> GMult a -> GMult a
pattern MultMul p q <- MultMul_ p q where
  One `MultMul` p = p
  p `MultMul` One = p
  Omega `MultMul` _ = Omega
  _ `MultMul` Omega = Omega
  p `MultMul` q = MultMul_ p q

pattern MultAdd :: GMult a -> GMult a -> GMult a
pattern MultAdd p q <- MultAdd_ p q where
  One `MultAdd` One = Omega
  Omega `MultAdd` _ = Omega
  _ `MultAdd` Omega = Omega
  p `MultAdd` q = MultAdd_ p q

pattern MultThing :: Multable a => a -> GMult a
pattern MultThing a <- MultThing_ a where
  MultThing a = toMult a

{-# COMPLETE One, Omega, MultMul, MultAdd, MultThing #-}

-- | Used to defined 'Multable' instances. Requires that the argument cannot be
-- reified any further. There is probably no good reason to use it outside of a
-- 'Multable' instance definition.
unsafeMultThing :: a -> GMult a
unsafeMultThing = MultThing_

instance (Outputable a, Multable a) => Outputable (GMult a) where
  ppr One = text "1"
  ppr Omega = text "Omega"
  ppr (MultAdd m1 m2) = parens (ppr m1 <+> text "+" <+> ppr m2)
  ppr (MultMul m1 m2) = parens (ppr m1 <+> text "*" <+> ppr m2)
  ppr (MultThing t) = ppr t

-- | @sup w1 w2@ returns the smallest multiplicity larger than or equal to both @w1@
-- and @w2@.
sup :: GMult a -> GMult a -> GMult a
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
data GScaled t a = Scaled {scaledMult :: GMult t, scaledThing :: a}
  deriving (Functor,Foldable,Traversable,Data)

unrestricted, linear, tymult :: a -> GScaled t a
unrestricted = Scaled Omega
linear = Scaled One

-- Used for type arguments in core
tymult = Scaled Omega

irrelevantMult :: GScaled t a -> a
irrelevantMult = scaledThing

mkScaled :: GMult t -> a -> GScaled t a
mkScaled = Scaled

instance (Multable t, Outputable a) => Outputable (GScaled t a) where
   ppr (Scaled _cnt t) = ppr t
     -- Do not print the multiplicity here because it tends to be too verbose

scaledSet :: GScaled t a -> b -> GScaled t b
scaledSet x b = fmap (\_->b) x

scaleScaled :: GMult t -> GScaled t a -> GScaled t a
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

submult :: GMult t -> GMult t -> IsSubmult
submult _     Omega = Submult
submult Omega One   = NotSubmult
submult One   One   = Submult
-- The 1 <= p rule
submult One   _     = Submult
--    submult (MultThing t) (MultThing t') = Unknown
submult _     _     = Unknown

traverseMult :: (Multable t, Multable u, Applicative f) => (t -> f u) -> GMult t -> f (GMult u)
traverseMult _ One = pure One
traverseMult _ Omega = pure Omega
traverseMult f (MultThing t) = MultThing <$> f t
traverseMult f (MultAdd x y) = MultAdd <$> traverseMult f x <*> traverseMult f y
traverseMult f (MultMul x y) = MultMul <$> traverseMult f x <*> traverseMult f y

multThingList :: Multable t => (t -> a) -> GMult t -> [a]
multThingList f = go []
  where go acc One = acc
        go acc Omega = acc
        go acc (MultThing t) = f t : acc
        go acc (MultAdd x y) = go (go acc y) x
        go acc (MultMul x y) = go (go acc y) x

-- Not a Functor, since MultThing calls 'fromMult'.
mapMult :: (Multable t, Multable u) => (t -> u) -> GMult t -> GMult u
mapMult _ One = One
mapMult _ Omega = Omega
mapMult f (MultThing t) = MultThing (f t)
mapMult f (MultAdd x y) = MultAdd (mapMult f x) (mapMult f y)
mapMult f (MultMul x y) = MultMul (mapMult f x) (mapMult f y)
