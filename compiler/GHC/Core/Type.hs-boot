{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Type where

import GhcPrelude
import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.TyCo.Rep( Type, Coercion )
import Util

isPredTy     :: HasDebugCallStack => Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy    :: Type -> Type -> Type
mkCastTy   :: Type -> Coercion -> Type
piResultTy :: HasDebugCallStack => Type -> Type -> Type

eqType :: Type -> Type -> Bool
fastEqType :: Type -> Type -> Bool

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type
isRuntimeRepTy :: Type -> Bool
isMultiplicityTy :: Type -> Bool
isLiftedTypeKind :: Type -> Bool

splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])

mkTyConApp :: TyCon -> [Type] -> Type

partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])
