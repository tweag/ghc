{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module UsageEnv (UsageEnv, addUsage, multUsage, emptyUE, zeroUE, mkUE,
                 toListUE,
                 lookupUE, scaleUE, deleteUE, addUE, Usage(..), unitUE,
                 supUE, supUEs) where

import Data.Foldable
import GhcPrelude
import Multiplicity
import Name
import NameEnv
import Outputable
import Data.Data

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

-- TODO describe Bottom
data Usage = Zero | Bottom | MUsage Mult

instance Outputable Usage where
  ppr Zero = text "0"
  ppr Bottom = text "Bottom"
  ppr (MUsage x) = ppr x

addUsage :: Usage -> Usage -> Usage
addUsage Bottom x = x
addUsage x Bottom = x
addUsage Zero x = x
addUsage x Zero = x
addUsage (MUsage x) (MUsage y) = MUsage $ mkMultAdd x y

multUsage :: Usage -> Usage -> Usage
multUsage Zero _ = Zero
multUsage _ Zero = Zero
multUsage Bottom _ = Bottom
multUsage _ Bottom = Bottom
multUsage (MUsage x) (MUsage y) = MUsage $ mkMultMul x y


-- If the boolean flag is True, then the usage environment
-- is the sum of NameEnv Mult and arbitrary multiplicities,
-- as in empty case (TODO explain).
--
-- We're storing names redundantly in the usage env so that we can serialise the
-- name env in Iface. That's because NameEnvs only store uniques rather than the
-- full name.
data UsageEnv = UsageEnv (NameEnv (Name, Mult)) Bool
  deriving (Data)

lift1 :: (Mult -> Mult) -> (Name, Mult) -> (Name, Mult)
lift1 f (n,p) = (n, f p)

lift2 :: (Mult -> Mult -> Mult) -> (Name, Mult) -> (Name, Mult) -> (Name, Mult)
lift2 f (n,p) (_,q) = (n, f p q)
  -- Because the name is assumed to be the same for a given unique, we can
  -- safely drop either name: both names are `n`!

unitUE :: NamedThing n => n -> Mult -> UsageEnv
unitUE x w = UsageEnv (unitNameEnv name (name, w)) False
  where
    name = getName x

zeroUE, emptyUE, bottomUE :: UsageEnv
zeroUE = UsageEnv emptyNameEnv False

emptyUE = zeroUE

bottomUE = UsageEnv emptyNameEnv True

addUE :: UsageEnv -> UsageEnv -> UsageEnv
addUE (UsageEnv e1 b1) (UsageEnv e2 b2) =
  UsageEnv (plusNameEnv_C (lift2 mkMultAdd) e1 e2) (b1 || b2)

mkUE :: [(Name, Mult)] -> UsageEnv
mkUE l = UsageEnv (mkNameEnv l') False
  where
    l' = [ (n, (n, p)) | (n,p) <- l ]

toListUE :: UsageEnv -> [(Name, Mult)]
toListUE (UsageEnv ue _) = nameEnvElts ue

scaleUE :: Mult -> UsageEnv -> UsageEnv
scaleUE w (UsageEnv e b) =
  UsageEnv (mapNameEnv (lift1 (mkMultMul w)) e) b

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1 False) (UsageEnv e2 False) =
  UsageEnv (plusNameEnv_CD2 combineUsage e1 e2) False
   where combineUsage (Just (n,p)) (Just (_,q)) = (n, mkMultSup p q)
         combineUsage Nothing (Just (n,p)) = (n, Omega)
         combineUsage (Just (n,p)) Nothing = (n, Omega)
         combineUsage Nothing Nothing = pprPanic "supUE" (ppr e1 <+> ppr e2)
supUE (UsageEnv e1 b1) (UsageEnv e2 b2) = UsageEnv (plusNameEnv_CD2 combineUsage e1 e2) (b1 && b2)
   where combineUsage (Just (n,p)) (Just (_,q)) = (n, mkMultSup p q)
         combineUsage Nothing  (Just x) | b1        = x
                                        | otherwise = (fst x, Omega)
         combineUsage (Just x) Nothing  | b2        = x
                                        | otherwise = (fst x, Omega)
         combineUsage Nothing  Nothing  = pprPanic "supUE" (ppr e1 <+> ppr e2)
-- Note: If you are changing this logic, check 'mkMultSup' in Multiplicity as well.

supUEs :: [UsageEnv] -> UsageEnv
supUEs = foldr supUE bottomUE
-- supUEs [] = bottomUE
-- supUEs l = foldr1 supUE l


deleteUE :: NamedThing n => UsageEnv -> n -> UsageEnv
deleteUE (UsageEnv e b) x = UsageEnv (delFromNameEnv e (getName x)) b

-- | |lookupUE x env| returns the multiplicity assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero| or |Bottom|.
lookupUE :: NamedThing n => UsageEnv -> n -> Usage
lookupUE (UsageEnv e has_bottom) x =
  case lookupNameEnv e (getName x) of
    Just (_,w)  -> MUsage w
    Nothing -> if has_bottom then Bottom else Zero

instance Outputable UsageEnv where
  ppr (UsageEnv ne b) = text "UsageEnv:" <+> ppr ne <+> ppr b
