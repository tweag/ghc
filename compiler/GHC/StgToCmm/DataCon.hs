{-# LANGUAGE CPP #-}
{-# OPTIONS -O -ddump-to-file -dumpdir datacondumps -ddump-simpl -ddump-stg #-}
-- {-# OPTIONS -dsuppress-all #-}

-----------------------------------------------------------------------------
--
-- Stg to C--: code generation for constructors
--
-- This module provides the support code for StgToCmm to deal with with
-- constructors on the RHSs of let(rec)s.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.DataCon (
        cgTopRhsCon, buildDynCon, bindConArgs
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Stg.Syntax
import GHC.Core  ( AltCon(..) )

import GHC.StgToCmm.Monad
import GHC.StgToCmm.Env
import GHC.StgToCmm.Heap
import GHC.StgToCmm.Layout
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Closure

import GHC.Cmm.Expr
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Cmm.Graph
import GHC.Runtime.Heap.Layout
import GHC.Types.CostCentre
import GHC.Types.Module
import GHC.Core.DataCon
import GHC.Driver.Session
import FastString
import GHC.Types.Id
import GHC.Types.Id.Info( CafInfo( NoCafRefs ) )
import GHC.Types.Name (isInternalName)
import GHC.Types.RepType (countConRepArgs)
import GHC.Types.Literal
import PrelInfo
import Outputable
import GHC.Platform
import Util
import MonadUtils (mapMaybeM)

import Control.Monad
import Data.Char

---------------------------------------------------------------
--      Top-level constructors
---------------------------------------------------------------

cgTopRhsCon :: DynFlags
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [NonVoid StgArg] -- Args
            -> (CgIdInfo, FCode ())
cgTopRhsCon dflags id con args
  | Just static_info <- precomputedStaticConInfo_maybe dflags id con args
  , let static_code | isInternalName name = pure ()
                    | otherwise           = gen_code
  = -- There is a pre-allocated static closure available; use it
    -- See Note [Precomputed static closures].
    -- For External bindings we must keep the binding,
    -- since importing modules will refer to it by name;
    -- but for Internal ones we can drop it altogether
    -- See Note [About the NameSorts] in Name.hs for Internal/External
    (static_info, static_code)

  -- Otherwise generate a closure for the constructor.
  | otherwise
  = (id_Info, gen_code)

  where
   id_Info       = litIdInfo dflags id (mkConLFInfo con) (CmmLabel closure_label)
   name          = idName id
   caffy         = idCafInfo id -- any stgArgHasCafRefs args
   closure_label = mkClosureLabel name caffy

   gen_code =
     do { this_mod <- getModuleName
        ; when (platformOS (targetPlatform dflags) == OSMinGW32) $
              -- Windows DLLs have a problem with static cross-DLL refs.
              MASSERT( not (isDllConApp dflags this_mod con (map fromNonVoid args)) )
        ; ASSERT( args `lengthIs` countConRepArgs con ) return ()

        -- LAY IT OUT
        ; let
            (tot_wds, --  #ptr_wds + #nonptr_wds
             ptr_wds, --  #ptr_wds
             nv_args_w_offsets) =
                 mkVirtHeapOffsetsWithPadding dflags StdHeader (addArgReps args)

            mk_payload (Padding len _) = return (CmmInt 0 (widthFromBytes len))
            mk_payload (FieldOff arg _) = do
                amode <- getArgAmode arg
                case amode of
                  CmmLit lit -> return lit
                  _          -> panic "GHC.StgToCmm.DataCon.cgTopRhsCon"

            nonptr_wds = tot_wds - ptr_wds

             -- we're not really going to emit an info table, so having
             -- to make a CmmInfoTable is a bit overkill, but mkStaticClosureFields
             -- needs to poke around inside it.
            info_tbl = mkDataConInfoTable dflags con True ptr_wds nonptr_wds


        ; payload <- mapM mk_payload nv_args_w_offsets
                -- NB1: nv_args_w_offsets is sorted into ptrs then non-ptrs
                -- NB2: all the amodes should be Lits!
                --      TODO (osa): Why?

                -- BUILD THE OBJECT
        ; emitDataCon closure_label info_tbl dontCareCCS payload }


---------------------------------------------------------------
--      Lay out and allocate non-top-level constructors
---------------------------------------------------------------

buildDynCon :: Id                 -- Name of the thing to which this constr will
                                  -- be bound
            -> Bool               -- is it genuinely bound to that name, or just
                                  -- for profiling?
            -> CostCentreStack    -- Where to grab cost centre from;
                                  -- current CCS if currentOrSubsumedCCS
            -> DataCon            -- The data constructor
            -> [NonVoid StgArg]   -- Its args
            -> FCode (CgIdInfo, FCode CmmAGraph)
               -- Return details about how to find it and initialization code
buildDynCon binder actually_bound cc con args
    = do dflags <- getDynFlags
         buildDynCon' dflags binder actually_bound cc con args


buildDynCon' :: DynFlags
             -> Id -> Bool
             -> CostCentreStack
             -> DataCon
             -> [NonVoid StgArg]
             -> FCode (CgIdInfo, FCode CmmAGraph)

{- We used to pass a boolean indicating whether all the
args were of size zero, so we could use a static
constructor; but I concluded that it just isn't worth it.
Now I/O uses unboxed tuples there just aren't any constructors
with all size-zero args.

The reason for having a separate argument, rather than looking at
the addr modes of the args is that we may be in a "knot", and
premature looking at the args will cause the compiler to black-hole!
-}

buildDynCon' dflags binder _ _cc con args
  | Just cgInfo <- precomputedStaticConInfo_maybe dflags binder con args
  -- , pprTrace "noCodeLocal:" (ppr (binder,con,args,cgInfo)) True
  = return (cgInfo, return mkNop)

-------- buildDynCon': the general case -----------
buildDynCon' dflags binder actually_bound ccs con args
  = do  { (id_info, reg) <- rhsIdInfo binder lf_info
        ; return (id_info, gen_code reg)
        }
 where
  lf_info = mkConLFInfo con

  gen_code reg
    = do  { let (tot_wds, ptr_wds, args_w_offsets)
                  = mkVirtConstrOffsets dflags (addArgReps args)
                nonptr_wds = tot_wds - ptr_wds
                info_tbl = mkDataConInfoTable dflags con False
                                ptr_wds nonptr_wds
          ; let ticky_name | actually_bound = Just binder
                           | otherwise = Nothing

          ; hp_plus_n <- allocDynClosure ticky_name info_tbl lf_info
                                          use_cc blame_cc args_w_offsets
          ; return (mkRhsInit dflags reg lf_info hp_plus_n) }
    where
      use_cc      -- cost-centre to stick in the object
        | isCurrentCCS ccs = cccsExpr
        | otherwise        = panic "buildDynCon: non-current CCS not implemented"

      blame_cc = use_cc -- cost-centre on which to blame the alloc (same)

{- Note [Precomputed static closures]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For Char/Int closures there are some value closures
built into the RTS. This is the case for all values in
the range mINT_INTLIKE .. mAX_INTLIKE (or CHARLIKE).
See Note [CHARLIKE and INTLIKE closures.] in the RTS code.

Similarly zero-arity constructors have a closure
in their defining Module we can use.

If possible we prefer to refer to those existing
closure instead of building new ones.

This is true at compile time where we do this replacement
in this module.
But also at runtime where the GC does the same (but only for
INT/CHAR closures).

`precomputedStaticConInfo_maybe` checks if a given constructor application
can be replaced with a reference to a existing static closure.

If so the code will reference the existing closure when accessing
the binding.
Unless the binding is visible to other modules we also generate
no code for the binding itself. We can do this since then we can
always reference the existing closure.

See Note [About the NameSorts] for the definition of external names.
For external bindings we must still generate a closure,
but won't use it inside this module.
This can sometimes reduce cache pressure. Since:
* If somebody uses the exported binding:
  + This module will reference the existing closure.
  + GC will reference the existing closure.
  + The importing module will reference the built closure.
* If nobody uses the exported binding:
  + This module will reference the RTS closures.
  + GC references the RTS closures

In the later case we avoided loading the built closure into the cache which
is what we optimize for here.

Consider this example using Ints.

    module M(externalInt, foo, bar) where

    externalInt = 1 :: Int
    internalInt = 1 :: Int
    { -# NOINLINE foo #- }
    foo = Just internalInt :: Maybe Int
    bar = Just externalInt

    ==================== STG: ====================
    externalInt = I#! [1#];

    bar = Just! [externalInt];

    internalInt_rc = I#! [2#];

    foo = Just! [internalInt_rc];

For externally visible bindings we must generate closures
since those may be referenced by their symbol `<name>_closure`
when imported.

`externalInt` is visible to other modules so we generate a closure:

    [section ""data" . M.externalInt_closure" {
        M.externalInt_closure:
            const GHC.Types.I#_con_info;
            const 1;
    }]

It will be referenced inside this module via `M.externalInt_closure+1`

`internalInt` is however a internal name. As such we generate no code for
it. References to it are replaced with references to the static closure as
we can see in the closure built for `foo`:

    [section ""data" . M.foo_closure" {
        M.foo_closure:
            const GHC.Maybe.Just_con_info;
            const stg_INTLIKE_closure+289; // == I# 2
            const 3;
    }]

This holds for both local and top level bindings.

We don't support this optimization when compiling into Windows DLLs yet
because they don't support cross package data references well.
-}

-- (precomputedStaticConInfo_maybe dflags id con args)
--     returns (Just cg_id_info)
-- if there is a precomputed static closure for (con args).
-- In that case, cg_id_info addresses it.
-- See Note [Precomputed static closures]
precomputedStaticConInfo_maybe :: DynFlags -> Id -> DataCon -> [NonVoid StgArg] -> Maybe CgIdInfo
precomputedStaticConInfo_maybe dflags binder con []
-- Nullary constructors
  | isNullaryRepDataCon con
  = Just $ litIdInfo dflags binder (mkConLFInfo con)
                (CmmLabel (mkClosureLabel (dataConName con) NoCafRefs))
precomputedStaticConInfo_maybe dflags binder con [arg]
  -- Int/Char values with existing closures in the RTS
  | intClosure || charClosure
  , platformOS platform /= OSMinGW32 || not (positionIndependent dflags)
  , Just val <- getClosurePayload arg
  , inRange val
  = let intlike_lbl   = mkCmmClosureLabel rtsUnitId (fsLit label)
        val_int = fromIntegral val :: Int
        offsetW = (val_int - (fromIntegral min_static_range)) * (fixedHdrSizeW dflags + 1)
                -- INTLIKE/CHARLIKE closures consist of a header and one word payload
        static_amode = cmmLabelOffW platform intlike_lbl offsetW
    in Just $ litIdInfo dflags binder (mkConLFInfo con) static_amode
  where
    platform = targetPlatform dflags
    intClosure = maybeIntLikeCon con
    charClosure = maybeCharLikeCon con
    getClosurePayload (NonVoid (StgLitArg (LitNumber LitNumInt val _))) = Just val
    getClosurePayload (NonVoid (StgLitArg (LitChar val))) = Just $ (fromIntegral . ord $ val)
    getClosurePayload _ = Nothing
    -- Avoid over/underflow by comparisons at type Integer!
    inRange :: Integer -> Bool
    inRange val
      = val >= min_static_range && val <= max_static_range

    min_static_range :: Integer
    min_static_range
      | intClosure = fromIntegral (mIN_INTLIKE dflags)
      | charClosure = fromIntegral (mIN_CHARLIKE dflags)
      | otherwise = panic "precomputedStaticConInfo_maybe: Unknown closure type"
    max_static_range
      | intClosure = fromIntegral (mAX_INTLIKE dflags)
      | charClosure = fromIntegral (mAX_CHARLIKE dflags)
      | otherwise = panic "precomputedStaticConInfo_maybe: Unknown closure type"
    label
      | intClosure = "stg_INTLIKE"
      | charClosure =  "stg_CHARLIKE"
      | otherwise = panic "precomputedStaticConInfo_maybe: Unknown closure type"

precomputedStaticConInfo_maybe _ _ _ _ = Nothing

---------------------------------------------------------------
--      Binding constructor arguments
---------------------------------------------------------------

bindConArgs :: AltCon -> LocalReg -> [NonVoid Id] -> FCode [LocalReg]
-- bindConArgs is called from cgAlt of a case
-- (bindConArgs con args) augments the environment with bindings for the
-- binders args, assuming that we have just returned from a 'case' which
-- found a con
bindConArgs (DataAlt con) base args
  = ASSERT(not (isUnboxedTupleCon con))
    do dflags <- getDynFlags
       platform <- getPlatform
       let (_, _, args_w_offsets) = mkVirtConstrOffsets dflags (addIdReps args)
           tag = tagForCon dflags con

           -- The binding below forces the masking out of the tag bits
           -- when accessing the constructor field.
           bind_arg :: (NonVoid Id, ByteOff) -> FCode (Maybe LocalReg)
           bind_arg (arg@(NonVoid b), offset)
             | isDeadBinder b  -- See Note [Dead-binder optimisation] in GHC.StgToCmm.Expr
             = return Nothing
             | otherwise
             = do { emit $ mkTaggedObjectLoad platform (idToReg platform arg)
                                              base offset tag
                  ; Just <$> bindArgToReg arg }

       mapMaybeM bind_arg args_w_offsets

bindConArgs _other_con _base args
  = ASSERT( null args ) return []
