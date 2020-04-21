module GHC.Tc.Utils.Unify where

import GhcPrelude
import GHC.Tc.Utils.TcType   ( TcTauType )
import GHC.Tc.Types          ( TcM )
import GHC.Tc.Types.Evidence ( TcCoercion, HsWrapper )
import GHC.Tc.Types.Origin ( CtOrigin )
import GHC.Hs.Expr      ( HsExpr )
import GHC.Hs.Types     ( HsType, Mult )
import GHC.Hs.Extension ( GhcRn )

-- This boot file exists only to tie the knot between
--              GHC.Tc.Utils.Unify and Inst

unifyType :: Maybe (HsExpr GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Maybe (HsType GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion

tcSubMult :: CtOrigin -> Mult -> Mult -> TcM HsWrapper
