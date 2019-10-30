{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module UsageEnv (UsageEnv, addUsage, scaleUsage, zeroUE,
                 lookupUE, scaleUE, deleteUE, deleteUEs, addUE, addUEs, Usage(..), unitUE,
                 supUE, supUEs, substUE,
                 UsageAnnotation, mapUA, traverseUA, zeroUA, addUA, scaleUA, interpUA, mkUA,
                 deleteUAs,
                 toListUA, fromListUA, substUA, nullUA,
                 compatibleUA,
                 UsageSubst) where

import Data.Foldable
import Data.Maybe
import GhcPrelude

import {-# SOURCE #-} TyCoRep (Type)
import Multiplicity
import Name
import NameEnv
import UniqFM
import Outputable

--
-- * Usage environments
--

-- | Like in the mathematical presentation, we have a context on which the
-- semi-ring of multiplicities acts (that is, 'UsageEnv' is a 'Mult'-module). Unlike the
-- mathematical presentation they are not type contexts, but only contain
-- multiplicities corresponding to the multiplicity required for a given variable in a
-- type-checked expression. The reason is twofold: it interacts less with the
-- rest of the type-checking infrastructure so it is easier to fit into the
-- existing implementation, and it is always an inferred datum (in the sense of
-- bidirectional type checking, i.e. it is an output of the type-checking
-- procedure) which makes it possible to use addition and scaling like in the
-- mathematical presentation, rather than subtraction and division which are
-- much harder to get right. The module structure is the point-wise extension of
-- the action of 'Mult' on itself, every absent name being considered to map to
-- 'Zero'.
data Usage = Zero | Bottom | MUsage Mult

instance Outputable Usage where
  ppr Zero = text "0"
  ppr Bottom = text "Bottom"
  ppr (MUsage x) = ppr x

addUsage :: Usage -> Usage -> Usage
addUsage Zero x = x
addUsage x Zero = x
addUsage Bottom x = x
addUsage x Bottom = x
addUsage (MUsage x) (MUsage y) = MUsage $ mkMultAdd x y

scaleUsage :: Mult -> Usage -> Usage
scaleUsage One Bottom     = Bottom
scaleUsage _   Zero       = Zero
scaleUsage x   Bottom     = MUsage x
scaleUsage x   (MUsage y) = MUsage $ mkMultMul x y

-- For now, we use extra multiplicity Bottom for empty case.
-- TODO: change to keeping UsageEnv on Case, issue #25.
-- If the boolean flag is True, then the usage environment
-- is the sum of NameEnv Mult and arbitrary multiplicities,
-- as in empty case.
data UsageEnv = UsageEnv (NameEnv (Name, Mult)) Bool

unitUE :: NamedThing n => n -> Mult -> UsageEnv
unitUE x w = UsageEnv (unitNameEnv n (n, w)) False
  where
    n = getName x

zeroUE, bottomUE :: UsageEnv
zeroUE = UsageEnv emptyNameEnv False

bottomUE = UsageEnv emptyNameEnv True

addUE :: UsageEnv -> UsageEnv -> UsageEnv
addUE (UsageEnv e1 b1) (UsageEnv e2 b2) =
    UsageEnv (plusNameEnv_C add e1 e2) (b1 || b2)
  where
    add (n,w) (_,w') = (n, mkMultAdd w w')

addUEs :: [UsageEnv] -> UsageEnv
addUEs = foldr addUE zeroUE

scaleUE :: Mult -> UsageEnv -> UsageEnv
scaleUE One ue = ue
scaleUE w (UsageEnv e _) =
  UsageEnv (mapNameEnv (fmap (mkMultMul w)) e) False

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1 b1) (UsageEnv e2 b2) = UsageEnv (plusNameEnv_CD2 combineUsage e1 e2) (b1 && b2)
   where combineUsage (Just (n,x)) (Just (_,y)) = (n, mkMultSup x y)
         combineUsage Nothing  (Just (n,x)) | b1        = (n, x)
                                            | otherwise = (n, Omega)
         combineUsage (Just (n, x)) Nothing  | b2        = (n, x)
                                             | otherwise = (n, Omega)
         combineUsage Nothing  Nothing  = pprPanic "supUE" (ppr e1 <+> ppr e2)
-- Note: If you are changing this logic, check 'mkMultSup' in Multiplicity as well.

supUEs :: [UsageEnv] -> UsageEnv
supUEs = foldr supUE bottomUE


deleteUE :: NamedThing n => UsageEnv -> n -> UsageEnv
deleteUE (UsageEnv e b) x = UsageEnv (delFromNameEnv e (getName x)) b

deleteUEs :: NamedThing n => UsageEnv -> [n] -> UsageEnv
deleteUEs = foldl deleteUE

-- | |lookupUE x env| returns the multiplicity assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero| or |Bottom|.
lookupUE :: NamedThing n => UsageEnv -> n -> Usage
lookupUE (UsageEnv e has_bottom) x =
  case lookupNameEnv e (getName x) of
    Just (_, w)  -> MUsage w
    Nothing -> if has_bottom then Bottom else Zero

instance Outputable UsageEnv where
  ppr (UsageEnv ne b) = text "UsageEnv:" <+> ppr ne <+> ppr b

--
-- * Usage annotation
--

-- See Note [Multiplicity of let binders] in Var

data UsageAnnotation = Ann [(Name, Mult)] Bool

instance Outputable UsageAnnotation where
  ppr (Ann anns b) = text "Usage anns:" <+> ppr anns <+> ppr b

mapUA :: (Type -> Type) -> UsageAnnotation -> UsageAnnotation
mapUA f (Ann us b) =
  Ann
    (map (fmap f) us)
    b

traverseUA :: Applicative f => (Type -> f Type) -> UsageAnnotation -> f UsageAnnotation
traverseUA f (Ann us b) =
  Ann
    <$> (traverse (traverse f) us)
    <*> pure b

-- | If the usage annotation is trivial, then there is no need to substitute in
-- it. It's a very quick test.
nullUA :: UsageAnnotation -> Bool
nullUA (Ann ua _) = null ua

-- | @compatibleUA ann computed@ checks that all the multiplicities in
-- @computed@ are submultiplicities of the corresponding multiplicity in @ann@.
compatibleUA :: UsageAnnotation -> UsageAnnotation -> Bool
compatibleUA (Ann anns_ann b_ann) (Ann anns_computed b_computed) =
  all (\(n_ann,w_ann) -> compatible w_ann (lookup n_ann anns_computed))
      anns_ann
  && (not b_ann) || b_computed
  where
    compatible _ Nothing = False
    compatible w_ann (Just w_computed) = submult w_computed w_ann == Submult

zeroUA :: UsageAnnotation
zeroUA = Ann [] False

addUA :: UsageEnv -> UsageAnnotation -> UsageAnnotation
addUA ue ua = mkUA $ ue `addUE` (interpUA ua)

scaleUA :: Mult -> UsageAnnotation -> UsageAnnotation
scaleUA w ua = mkUA $ w `scaleUE` (interpUA ua)

deleteUAs :: NamedThing n => UsageAnnotation -> [n] -> UsageAnnotation
deleteUAs ua ns = mkUA $ (interpUA ua) `deleteUEs` ns

interpUA :: UsageAnnotation -> UsageEnv
interpUA (Ann us b) =
  UsageEnv
    (mkNameEnv (map (\(n,w) -> (n, (n,w))) us))
    b

-- TODO: make deterministic
mkUA :: UsageEnv -> UsageAnnotation
mkUA (UsageEnv vars b) =
  Ann
    (filter (\case { (_, Omega) -> False; _ -> True}) (nameEnvElts vars))
    b

toListUA :: UsageAnnotation -> ([(Name, Mult)], Bool)
toListUA (Ann anns b) = (anns, b)

fromListUA :: ([(Name, Mult)], Bool) -> UsageAnnotation
fromListUA (anns, b) = Ann anns b

--
-- * Substitutions
--

type UsageSubst = NameEnv UsageEnv

subst_in_ue :: UsageSubst -> Name -> UsageEnv
subst_in_ue subst n =
  case lookupNameEnv subst n of
    Just ue -> ue
    Nothing -> unitUE n One

substUE :: UsageSubst -> UsageEnv ->  UsageEnv
substUE subst (UsageEnv ue b) = adjust_bottom $ foldl' subst_one zeroUE (NonDetUniqFM ue)
  where
    subst_one :: UsageEnv -> (Name, Mult) -> UsageEnv
    subst_one ue' (n,w) = (w `scaleUE` subst_in_ue subst n) `addUE` ue'

    adjust_bottom (UsageEnv ue' _) = UsageEnv ue' b

substUA :: UsageSubst -> UsageAnnotation -> UsageAnnotation
substUA subst ua = mkUA (substUE subst (interpUA ua))
