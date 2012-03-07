%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}

module CgCase (
        cgCase,
        saveVolatileVarsAndRegs,
        restoreCurrentCostCentre
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr ( cgExpr )

import CgMonad
import CgBindery
import CgCon
import CgHeapery
import CgCallConv
import CgStackery
import CgTailCall
import CgPrimOp
import CgForeignCall
import CgUtils
import CgProf
import CgInfoTbls

import ClosureInfo
import OldCmmUtils
import OldCmm

import StgSyn
import StaticFlags
import Id
import ForeignCall
import VarSet
import CoreSyn
import PrimOp
import Type
import TyCon
import Util
import UniqSupply
import MonadUtils
import Outputable
import FastString

import Control.Monad
\end{code}

\begin{code}
data GCFlag
  = GCMayHappen -- The scrutinee may involve GC, so everything must be
                -- tidy before the code for the scrutinee.

  | NoGC        -- The scrutinee is a primitive value, or a call to a
                -- primitive op which does no GC.  Hence the case can
                -- be done inline, without tidying up first.
\end{code}

It is quite interesting to decide whether to put a heap-check
at the start of each alternative.  Of course we certainly have
to do so if the case forces an evaluation, or if there is a primitive
op which can trigger GC.

A more interesting situation is this:

 \begin{verbatim}
        !A!;
        ...A...
        case x# of
          0#      -> !B!; ...B...
          default -> !C!; ...C...
 \end{verbatim}

where \tr{!x!} indicates a possible heap-check point. The heap checks
in the alternatives {\em can} be omitted, in which case the topmost
heapcheck will take their worst case into account.

In favour of omitting \tr{!B!}, \tr{!C!}:

 - {\em May} save a heap overflow test,
        if ...A... allocates anything.  The other advantage
        of this is that we can use relative addressing
        from a single Hp to get at all the closures so allocated.

 - No need to save volatile vars etc across the case

Against:

  - May do more allocation than reqd.  This sometimes bites us
        badly.  For example, nfib (ha!)  allocates about 30\% more space if the
        worst-casing is done, because many many calls to nfib are leaf calls
        which don't need to allocate anything.

        This never hurts us if there is only one alternative.

\begin{code}
cgCase  :: StgExpr
        -> StgLiveVars
        -> StgLiveVars
        -> Id
        -> AltType
        -> [StgAlt]
        -> Code
\end{code}

Special case #1: case of literal.

\begin{code}
cgCase (StgLit lit) _live_in_whole_case _live_in_alts bndr
       alt_type@(PrimAlt _) alts
  = do  { [tmp_reg] <- bindNewToTemp bndr
        ; cm_lit <- cgLit lit
        ; stmtC (CmmAssign (CmmLocal tmp_reg) (CmmLit cm_lit))
        ; cgPrimAlts NoGC alt_type [CmmLocal tmp_reg] alts }
\end{code}

Special case #2: scrutinising a primitive-typed variable.       No
evaluation required.  We don't save volatile variables, nor do we do a
heap-check in the alternatives.  Instead, the heap usage of the
alternatives is worst-cased and passed upstream.  This can result in
allocating more heap than strictly necessary, but it will sometimes
eliminate a heap check altogether.

\begin{code}
cgCase (StgApp v []) _live_in_whole_case _live_in_alts bndr
       alt_type alts
  | case alt_type of PrimAlt _ -> True; UbxTupAlt _ -> True; _ -> False
  -- Note [ticket #3132]: we might be looking at a case of a lifted Id
  -- that was cast to an unlifted type.  The Id will always be bottom,
  -- but we don't want the code generator to fall over here.  If we
  -- just emit an assignment here, the assignment will be
  -- type-incorrect Cmm.  Hence we check that the types match, and if
  -- they don't we'll fall through and emit the usual enter/return
  -- code.  Test case: codeGen/should_compile/3132.hs
  , isUnLiftedType (idType v)

  -- However, we also want to allow an assignment to be generated
  -- in the case when the types are compatible, because this allows
  -- some slightly-dodgy but occasionally-useful casts to be used,
  -- such as in RtClosureInspect where we cast an HValue to a MutVar#
  -- so we can print out the contents of the MutVar#.  If we generate
  -- code that enters the HValue, then we'll get a runtime panic, because
  -- the HValue really is a MutVar#.  The types are compatible though,
  -- so we can just generate an assignment.
  || reps_compatible
  =  WARN( null (idCgRep v), ptext (sLit "Case of void constant; missing optimisation somewhere") <+> ppr bndr)
     do { when (not reps_compatible) $
            panic "cgCase: reps do not match, perhaps a dodgy unsafeCoerce?"

          -- TODO: could just bind the default binder to the same thing as the scrutinee,
          -- rather than allocating these temporaries.
          -- Having two Ids share locations doesn't confuse nukeDeadBindings any longer.
        ; (tmp_regs, do_rhs) <- case alt_type of
            PrimAlt _ -> do
                tmp_regs <- bindNewToTemp bndr
                return (tmp_regs, cgPrimAlts NoGC alt_type (map CmmLocal tmp_regs) alts)
            UbxTupAlt _
              | [(DEFAULT, [], _, rhs)]     <- alts -> do
                tmp_regs <- bindNewToTemp bndr
                return (tmp_regs, cgExpr rhs)
              | [(DataAlt _, args, _, rhs)] <- alts -> do
                tmp_regss <- mapM bindNewToTemp args
                bindToRegs bndr (concat tmp_regss)
                return (concat tmp_regss, cgExpr rhs)
            _  -> panic "cgCase: weird UbxTupAlt?"

        ; v_info <- getCgIdInfo v
        ; amodes <- idInfoToAmodes v_info
        ; forM_ (zipEqual "cgCase" tmp_regs amodes) $ \(tmp_reg, amode) -> stmtC (CmmAssign (CmmLocal tmp_reg) amode)
        ; do_rhs }
  where
    reps_compatible = idCgRep v == idCgRep bndr
\end{code}

Special case #2.5; seq#

  (taking advantage of the fact that the return convention for (# State#, a #)
  is the same as the return convention for just 'a')

\begin{code}
cgCase (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _)
       _live_in_whole_case live_in_alts bndr alt_type alts
  = do { fun_info <- getCgIdInfo a
       ; cgCaseOfApp fun_info [] live_in_alts bndr alt_type alts }
\end{code}

Special case #3: inline PrimOps and foreign calls.

\begin{code}
cgCase (StgOpApp (StgPrimOp primop) args _)
       _live_in_whole_case live_in_alts bndr alt_type alts
  | not (primOpOutOfLine primop)
  = cgInlinePrimOp primop args bndr alt_type live_in_alts alts
\end{code}

TODO: Case-of-case of primop can probably be done inline too (but
maybe better to translate it out beforehand).  See
ghc/lib/misc/PackedString.lhs for examples where this crops up (with
4.02).

Special case #4: inline foreign calls: an unsafe foreign call can be done
right here, just like an inline primop.

\begin{code}
cgCase (StgOpApp (StgFCallOp fcall _) args _)
       _live_in_whole_case live_in_alts _bndr _alt_type alts
  | unsafe_foreign_call
  = ASSERT( isSingleton alts )
    do  --  *must* be an unboxed tuple alt.
        -- exactly like the cgInlinePrimOp case for unboxed tuple alts..
        { res_tmps <- concatMapM bindNewToTemp res_ids
        ; let res_hints = concatMap (typeForeignHint.idType) res_ids
        ; cgForeignCall (zipWithEqual "cgCase" CmmHinted res_tmps res_hints) fcall args live_in_alts
        ; cgExpr rhs }
  where
   (_, res_ids, _, rhs) = head alts

   unsafe_foreign_call
         = case fcall of
                CCall (CCallSpec _ _ s) -> not (playSafe s)
\end{code}

Special case: scrutinising a non-primitive variable.
This can be done a little better than the general case, because
we can reuse/trim the stack slot holding the variable (if it is in one).

\begin{code}
cgCase (StgApp fun args)
        _live_in_whole_case live_in_alts bndr alt_type alts
  = do  { fun_info <- getCgIdInfo fun
        ; arg_amodes <- mapM getArgAmodes args
        ; cgCaseOfApp fun_info arg_amodes live_in_alts bndr alt_type alts }
\end{code}

Note about return addresses: we *always* push a return address, even
if because of an optimisation we end up jumping direct to the return
code (not through the address itself).  The alternatives always assume
that the return address is on the stack.  The return address is
required in case the alternative performs a heap check, since it
encodes the liveness of the slots in the activation record.

On entry to the case alternative, we can re-use the slot containing
the return address immediately after the heap check.  That's what the
deAllocStackTop call is doing above.

Finally, here is the general case.

\begin{code}
cgCase expr live_in_whole_case live_in_alts bndr alt_type alts
  = do  {       -- Figure out what volatile variables to save
          nukeDeadBindings live_in_whole_case

        ; (save_assts, alts_eob_info, maybe_cc_slot)
                <- saveVolatileVarsAndRegs live_in_alts

             -- Save those variables right now!
        ; emitStmts save_assts

            -- generate code for the alts
        ; scrut_eob_info
               <- forkEval alts_eob_info
                           (do  { nukeDeadBindings live_in_alts
                                ; allocStackTop retAddrSizeW   -- space for retn address
                                ; nopC })
                           (do  { deAllocStackTop retAddrSizeW
                                ; cgEvalAlts maybe_cc_slot bndr alt_type alts })

        ; setEndOfBlockInfo scrut_eob_info (cgExpr expr)
    }
\end{code}

\begin{code}
cgCaseOfApp :: CgIdInfo
            -> [[(CgRep, CmmExpr)]]
            -> StgLiveVars
            -> Id
            -> AltType
            -> [StgAlt]
            -> Code
cgCaseOfApp fun_info arg_amodes live_in_alts bndr alt_type alts
 = do { -- Nuking dead bindings *before* calculating the saves is the
        -- value-add here.  We might end up freeing up some slots currently
        -- occupied by variables only required for the call.
        -- NOTE: we need to look up the variables used in the call before
        -- doing this, because some of them may not be in the environment
        -- afterward.
        ; nukeDeadBindings live_in_alts
        ; (save_assts, alts_eob_info, maybe_cc_slot)
            <- saveVolatileVarsAndRegs live_in_alts

        ; scrut_eob_info
            <- forkEval alts_eob_info
                        (allocStackTop retAddrSizeW >> nopC)
                        (do { deAllocStackTop retAddrSizeW
                            ; cgEvalAlts maybe_cc_slot bndr alt_type alts })

        ; setEndOfBlockInfo scrut_eob_info
                            (performTailCall fun_info arg_amodes save_assts) }
\end{code}

There's a lot of machinery going on behind the scenes to manage the
stack pointer here.  forkEval takes the virtual Sp and free list from
the first argument, and turns that into the *real* Sp for the second
argument.  It also uses this virtual Sp as the args-Sp in the EOB info
returned, so that the scrutinee will trim the real Sp back to the
right place before doing whatever it does.
  --SDM (who just spent an hour figuring this out, and didn't want to
         forget it).

Why don't we push the return address just before evaluating the
scrutinee?  Because the slot reserved for the return address might
contain something useful, so we wait until performing a tail call or
return before pushing the return address (see
CgTailCall.pushReturnAddress).

This also means that the environment doesn't need to know about the
free stack slot for the return address (for generating bitmaps),
because we don't reserve it until just before the eval.

TODO!!  Problem: however, we have to save the current cost centre
stack somewhere, because at the eval point the current CCS might be
different.  So we pick a free stack slot and save CCCS in it.  One
consequence of this is that activation records on the stack don't
follow the layout of closures when we're profiling.  The CCS could be
anywhere within the record).

%************************************************************************
%*                                                                      *
                Inline primops
%*                                                                      *
%************************************************************************

\begin{code}
cgInlinePrimOp :: PrimOp -> [StgArg] -> Id -> AltType -> StgLiveVars
               -> [(AltCon, [Id], [Bool], StgExpr)]
               -> Code
cgInlinePrimOp primop args bndr (PrimAlt tycon) live_in_alts alts
  = do  {       -- PRIMITIVE ALTS, with void OR non-void result
          tmp_regs <- bindNewToTemp bndr
        ; cgPrimOp tmp_regs primop args live_in_alts
        ; cgPrimAlts NoGC (PrimAlt tycon) (map CmmLocal tmp_regs) alts }

cgInlinePrimOp primop args bndr (UbxTupAlt _) live_in_alts alts
  = do  {       -- UNBOXED TUPLE ALTS
                -- No heap check, no yield, just get in there and do it.

        ; (res_tmps, rhs) <- case alts of
            [(DEFAULT, [], _, rhs)] | Just (_, tys) <- splitTyConApp_maybe (idType bndr) -> do
                us <- newUniqSupply
                let res_tmps = zipWith LocalReg (uniqsFromSupply us) (concatMap (map (argMachRep . primRepToCgRep) . typePrimRep) tys)
                return (res_tmps, rhs)
            [(DataAlt _, res_ids, _, rhs)] -> do
                res_tmps <- concatMapM bindNewToTemp res_ids
                return (res_tmps, rhs)
            _ -> panic "cgInlinePrimOp"
        ; bindToRegs bndr res_tmps
        ; cgPrimOp res_tmps primop args live_in_alts
        ; cgExpr rhs }

cgInlinePrimOp primop args bndr (AlgAlt tycon) live_in_alts alts
  = do  {       -- ENUMERATION TYPE RETURN
                -- Typical: case a ># b of { True -> ..; False -> .. }
                -- The primop itself returns an index into the table of
                -- closures for the enumeration type.
           tag_amode <- ASSERT( isEnumerationTyCon tycon )
                        do_enum_primop primop

                -- Bind the default binder if necessary
                -- (avoiding it avoids the assignment)
                -- The deadness info is set by StgVarInfo
        ; whenC (not (isDeadBinder bndr))
                (do { [tmp_reg] <- bindNewToTemp bndr
                    ; stmtC (CmmAssign
                             (CmmLocal tmp_reg)
                             (tagToClosure tycon tag_amode)) })

                -- Compile the alts
        ; (branches, mb_deflt) <- cgAlgAlts NoGC Nothing{-cc_slot-}
                                            (AlgAlt tycon) alts

                -- Do the switch
        ; emitSwitch tag_amode branches mb_deflt 0 (tyConFamilySize tycon - 1)
        }
  where

    do_enum_primop :: PrimOp -> FCode CmmExpr   -- Returns amode for result
    do_enum_primop TagToEnumOp  -- No code!
       | [arg] <- args = do
         [(_,e)] <- getArgAmodes arg
         return e
    do_enum_primop primop
      = do tmp <- newTemp bWord
           cgPrimOp [tmp] primop args live_in_alts
           returnFC (CmmReg (CmmLocal tmp))

cgInlinePrimOp _ _ bndr _ _ _
  = pprPanic "cgCase: case of primop has polymorphic type" (ppr bndr)
\end{code}

%************************************************************************
%*                                                                      *
\subsection[CgCase-alts]{Alternatives}
%*                                                                      *
%************************************************************************

@cgEvalAlts@ returns an addressing mode for a continuation for the
alternatives of a @case@, used in a context when there
is some evaluation to be done.

\begin{code}
cgEvalAlts :: Maybe VirtualSpOffset     -- Offset of cost-centre to be restored, if any
           -> Id
           -> AltType
           -> [StgAlt]
           -> FCode Sequel      -- Any addr modes inside are guaranteed
                                -- to be a label so that we can duplicate it
                                -- without risk of duplicating code

cgEvalAlts cc_slot bndr alt_type@(PrimAlt tycon) alts
  = do  { let   reps = tyConCgRep tycon
                regs = case reps of []    -> []
                                    [rep] -> [dataReturnConvPrim rep]
                                    _     -> panic "cgEvalAlts"

        ; abs_c <- forkProc $ do
                {       -- Bind the case binder
                  bindNewToReg bndr (zipEqual "cgEvalAlts" regs (mkLFArgument (idType bndr)))
                ; restoreCurrentCostCentre cc_slot True
                ; cgPrimAlts GCMayHappen alt_type regs alts }

        ; lbl <- emitReturnTarget (idName bndr) abs_c
        ; returnFC (CaseAlts lbl Nothing bndr) }

cgEvalAlts cc_slot bndr (UbxTupAlt _) [(con,args,_,rhs)]
  = do  {       -- forkAbsC for the RHS, so that the envt is
                -- not changed for the emitReturn call
          abs_c <- forkProc $ do
                { (flat_arg_locs, live_regs, ptrs, nptrs) <- case con of
                        DEFAULT
                         | Just (_, tys) <- splitTyConApp_maybe (idType bndr)
                         , [] <- args -> do
                          (arg_locs, live_regs, ptrs, nptrs, _) <- bindUnboxedTupleComponents [((), typeCgRep ty) | ty <- tys]
                          return (concatMap snd arg_locs, live_regs, ptrs, nptrs)
                        DataAlt _ -> do
                          (arg_locs, live_regs, ptrs, nptrs, _) <- bindUnboxedTupleComponents [(arg, idCgRep arg) | arg <- args]
                          bindArgsToRegOrStack arg_locs
                          return (concatMap snd arg_locs, live_regs, ptrs, nptrs)
                        _ -> panic "cgEvalAlts"
                ; bindArgsToRegOrStack [(bndr, flat_arg_locs)]
                        -- Restore the CC *after* binding the tuple components,
                        -- so that we get the stack offset of the saved CC right.
                ; restoreCurrentCostCentre cc_slot True
                        -- Generate a heap check if necessary
                        -- and finally the code for the alternative
                ; unbxTupleHeapCheck live_regs ptrs nptrs noStmts
                                     (cgExpr rhs) }
        ; lbl <- emitReturnTarget (idName bndr) abs_c
        ; returnFC (CaseAlts lbl Nothing bndr) }

cgEvalAlts cc_slot bndr alt_type alts
  =     -- Algebraic and polymorphic case
    do  {       -- Bind the default binder
          bindNewToReg bndr [(nodeReg, only (mkLFArgument (idType bndr)))]

        -- Generate sequel info for use downstream
        -- At the moment, we only do it if the type is vector-returnable.
        -- Reason: if not, then it costs extra to label the
        -- alternatives, because we'd get return code like:
        --
        --      switch TagReg { 0 : JMP(alt_1); 1 : JMP(alt_2) ..etc }
        --
        -- which is worse than having the alt code in the switch statement

        ; (alts, mb_deflt) <- cgAlgAlts GCMayHappen cc_slot alt_type alts

        ; (lbl, branches) <- emitAlgReturnTarget (idName bndr)
                                alts mb_deflt fam_sz

        ; returnFC (CaseAlts lbl branches bndr) }
  where
    fam_sz = case alt_type of
                AlgAlt tc -> tyConFamilySize tc
                PolyAlt   -> 0
                PrimAlt _ -> panic "cgEvalAlts: PrimAlt"
                UbxTupAlt _ -> panic "cgEvalAlts: UbxTupAlt"
\end{code}


HWL comment on {\em GrAnSim\/}  (adding GRAN_YIELDs for context switch): If
we  do  an inlining of the  case  no separate  functions  for returning are
created, so we don't have to generate a GRAN_YIELD in that case.  This info
must be  propagated  to cgAlgAltRhs (where the  GRAN_YIELD  macro might  be
emitted). Hence, the new Bool arg to cgAlgAltRhs.

%************************************************************************
%*                                                                      *
\subsection[CgCase-alg-alts]{Algebraic alternatives}
%*                                                                      *
%************************************************************************

In @cgAlgAlts@, none of the binders in the alternatives are
assumed to be yet bound.

HWL comment on {\em GrAnSim\/} (adding GRAN_YIELDs for context switch): The
last   arg of  cgAlgAlts  indicates  if we  want  a context   switch at the
beginning of  each alternative. Normally we  want that. The  only exception
are inlined alternatives.

\begin{code}
cgAlgAlts :: GCFlag
       -> Maybe VirtualSpOffset
       -> AltType                               --  ** AlgAlt or PolyAlt only **
       -> [StgAlt]                              -- The alternatives
       -> FCode ( [(ConTagZ, CgStmts)], -- The branches
                  Maybe CgStmts )       -- The default case

cgAlgAlts gc_flag cc_slot alt_type alts
  = do alts <- forkAlts [ cgAlgAlt gc_flag cc_slot alt_type alt | alt <- alts]
       let
            mb_deflt = case alts of -- DEFAULT is always first, if present
                         ((DEFAULT,blks) : _) -> Just blks
                         _                    -> Nothing

            branches = [(dataConTagZ con, blks)
                       | (DataAlt con, blks) <- alts]
       -- in
       return (branches, mb_deflt)


cgAlgAlt :: GCFlag
         -> Maybe VirtualSpOffset       -- Turgid state
         -> AltType                     --  ** AlgAlt or PolyAlt only **
         -> StgAlt
         -> FCode (AltCon, CgStmts)

cgAlgAlt gc_flag cc_slot alt_type (con, args, _use_mask, rhs)
  = do  { abs_c <- getCgStmts $ do
                { bind_con_args con args
                ; restoreCurrentCostCentre cc_slot True
                ; maybeAltHeapCheck gc_flag alt_type (cgExpr rhs) }
        ; return (con, abs_c) }
  where
    bind_con_args DEFAULT      _    = nopC
    bind_con_args (DataAlt dc) args = bindConArgs dc args
    bind_con_args (LitAlt _)   _    = panic "cgAlgAlt: LitAlt"
\end{code}


%************************************************************************
%*                                                                      *
\subsection[CgCase-prim-alts]{Primitive alternatives}
%*                                                                      *
%************************************************************************

@cgPrimAlts@ generates suitable a @CSwitch@
for dealing with the alternatives of a primitive @case@, given an
addressing mode for the thing to scrutinise.  It also keeps track of
the maximum stack depth encountered down any branch.

As usual, no binders in the alternatives are yet bound.

\begin{code}
cgPrimAlts :: GCFlag
           -> AltType   -- Always PrimAlt, but passed to maybeAltHeapCheck
           -> [CmmReg]  -- Scrutinee registers: either unary or nullary (if void)
           -> [StgAlt]  -- Alternatives
           -> Code
-- NB: cgPrimAlts emits code that does the case analysis.
-- It's often used in inline situations, rather than to genearte
-- a labelled return point.  That's why its interface is a little
-- different to cgAlgAlts
--
-- INVARIANT: the default binder is already bound
cgPrimAlts gc_flag alt_type scrutinees alts
  = do  { tagged_absCs <- forkAlts (map (cgPrimAlt gc_flag alt_type) alts)
        ; let ((DEFAULT, deflt_absC) : others) = tagged_absCs   -- There is always a default
              alt_absCs = [(lit,rhs) | (LitAlt lit, rhs) <- others]
        ; case scrutinees of
            []      -> emitCgStmts deflt_absC
            [scrut] -> emitLitSwitch (CmmReg scrut) alt_absCs deflt_absC
            _       -> panic "cgPrimAlts: unboxed tuple scrutinee" }

cgPrimAlt :: GCFlag
          -> AltType
          -> StgAlt                             -- The alternative
          -> FCode (AltCon, CgStmts)    -- Its compiled form

cgPrimAlt gc_flag alt_type (con, [], [], rhs)
  = ASSERT( case con of { DEFAULT -> True; LitAlt _ -> True; _ -> False } )
    do  { abs_c <- getCgStmts (maybeAltHeapCheck gc_flag alt_type (cgExpr rhs))
        ; returnFC (con, abs_c) }
cgPrimAlt _ _ _ = panic "cgPrimAlt: non-empty lists"
\end{code}


%************************************************************************
%*                                                                      *
\subsection[CgCase-tidy]{Code for tidying up prior to an eval}
%*                                                                      *
%************************************************************************

\begin{code}
maybeAltHeapCheck
        :: GCFlag
        -> AltType      -- PolyAlt, PrimAlt, AlgAlt, but *not* UbxTupAlt
        -> Code         -- Continuation
        -> Code
maybeAltHeapCheck NoGC        _        code = code
maybeAltHeapCheck GCMayHappen alt_type code = altHeapCheck alt_type code

saveVolatileVarsAndRegs
    :: StgLiveVars                    -- Vars which should be made safe
    -> FCode (CmmStmts,               -- Assignments to do the saves
              EndOfBlockInfo,         -- sequel for the alts
              Maybe VirtualSpOffset)  -- Slot for current cost centre

saveVolatileVarsAndRegs vars
  = do  { var_saves <- saveVolatileVars vars
        ; (maybe_cc_slot, cc_save) <- saveCurrentCostCentre
        ; eob_info <- getEndOfBlockInfo
        ; returnFC (var_saves `plusStmts` cc_save,
                    eob_info,
                    maybe_cc_slot) }


saveVolatileVars :: StgLiveVars         -- Vars which should be made safe
                 -> FCode CmmStmts      -- Assignments to to the saves

saveVolatileVars vars
  = do  { stmts_s <- concatMapM save_it (varSetElems vars)
        ; return (foldr plusStmts noStmts stmts_s) }
  where
    save_it var
      = do { vol_amodes <- getVolatilesCAddrModes var -- If non-volatile, empty list
           ; (stmts, slots) <- liftM unzip $ forM vol_amodes $ \mb_vol_amode -> case mb_vol_amode of
                 Nothing -> return (noStmts, Nothing)
                 Just (rep, vol_amode) -> do
                   slot <- allocPrimStack rep
                   sp_rel <- getSpRelOffset slot
                   returnFC (oneStmt (CmmStore sp_rel vol_amode), Just slot)
           ; rebindToStack var slots
           ; return stmts }
\end{code}

---------------------------------------------------------------------------

When we save the current cost centre (which is done for lexical
scoping), we allocate a free stack location, and return (a)~the
virtual offset of the location, to pass on to the alternatives, and
(b)~the assignment to do the save (just as for @saveVolatileVars@).

\begin{code}
saveCurrentCostCentre ::
        FCode (Maybe VirtualSpOffset,   -- Where we decide to store it
               CmmStmts)                -- Assignment to save it

saveCurrentCostCentre
  | not opt_SccProfilingOn
  = returnFC (Nothing, noStmts)
  | otherwise
  = do  { slot <- allocPrimStack PtrArg
        ; sp_rel <- getSpRelOffset slot
        ; returnFC (Just slot,
                    oneStmt (CmmStore sp_rel curCCS)) }

-- Sometimes we don't free the slot containing the cost centre after restoring it
-- (see CgLetNoEscape.cgLetNoEscapeBody).
restoreCurrentCostCentre :: Maybe VirtualSpOffset -> Bool -> Code
restoreCurrentCostCentre Nothing     _freeit = nopC
restoreCurrentCostCentre (Just slot) freeit
 = do   { sp_rel <- getSpRelOffset slot
        ; whenC freeit (freeStackSlots [slot])
        ; stmtC (storeCurCCS (CmmLoad sp_rel bWord)) }
\end{code}

