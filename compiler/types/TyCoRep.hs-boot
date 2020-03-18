module TyCoRep where

import Outputable ( Outputable )
import Data.Data  ( Data )
import {-# SOURCE #-} Var( Var, ArgFlag, AnonArgFlag )

data Type
data TyThing
data Coercion
data UnivCoProvenance
data TyLit
data TyCoBinder
data MCoercion

type PredType = Type
type Kind = Type
type ThetaType = [PredType]
type CoercionN = Coercion
type MCoercionN = MCoercion

mkFunTyMany :: AnonArgFlag -> Type -> Type -> Type
mkForAllTy :: Var -> ArgFlag -> Type -> Type

instance Data Type  -- To support Data instances in CoAxiom
instance Outputable Type
