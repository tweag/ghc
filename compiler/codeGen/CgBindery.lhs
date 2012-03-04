%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgBindery]{Utility functions related to doing @CgBindings@}

\begin{code}

module CgBindery (
        CgBindings, CgIdInfo, CgIdElemInfo,
        StableLoc, VolatileLoc,

        cgIdInfoId, cgIdInfoElems, cgIdElemInfoArgRep, cgIdElemInfoLF,
        cgIdInfoSingleElem,

        stableIdInfo, heapIdInfo,
        taggedStableIdInfo, taggedHeapIdInfo,
        letNoEscapeIdInfo,
        idInfoToAmodes, idElemInfoToAmode,

        addBindC, addBindsC,

        nukeVolatileBinds,
        nukeDeadBindings,
        getLiveStackSlots,
        getLiveStackBindings,

        rebindToStack, bindArgsToRegOrStack,
        bindNewToNode, bindNewToUntagNode, bindNewToReg,
        bindNewToTemp, bindToRegs,
        getArgAmodes,
        getCgIdInfo, 
        getVolatilesCAddrModes, getVolatileRegs,
        maybeLetNoEscape, 
    ) where

#include "HsVersions.h"

import CgMonad
import CgHeapery
import CgStackery
import CgUtils
import CLabel
import ClosureInfo
import Constants

import OldCmm
import PprCmm           ( {- instance Outputable -} )
import SMRep
import Id
import DataCon
import VarEnv
import VarSet
import Literal
import Maybes
import Name
import StgSyn
import Unique
import UniqSet
import Outputable
import FastString
import Util
import UniqSupply

import Control.Monad
import Data.List

\end{code}

%************************************************************************
%*                                                                      *
\subsection[Bindery-datatypes]{Data types}
%*                                                                      *
%************************************************************************

@(CgBinding a b)@ is a type of finite maps from a to b.

The assumption used to be that @lookupCgBind@ must get exactly one
match. This is {\em completely wrong} in the case of compiling
letrecs (where knot-tying is used). An initial binding is fed in (and
never evaluated); eventually, a correct binding is put into the
environment. So there can be two bindings for a given name.

\begin{code}
type CgBindings = IdEnv CgIdInfo

data CgIdInfo
  = CgIdInfo    
        { cg_id :: Id   -- Id that this is the info for
                        -- Can differ from the Id at occurrence sites by 
                        -- virtue of being externalised, for splittable C
        , cg_elems :: [CgIdElemInfo]
        }

data CgIdElemInfo
  = CgIdElemInfo
        { cg_rep :: CgRep
        , cg_vol :: VolatileLoc
        , cg_stb :: StableLoc
        , cg_lf  :: LambdaFormInfo 
        , cg_tag :: {-# UNPACK #-} !Int  -- tag to be added in idInfoToAmode
         }

-- Used only for Id with a guaranteed-unary CgRep
mkCgIdInfo :: Id -> VolatileLoc -> StableLoc -> LambdaFormInfo -> CgIdInfo
mkCgIdInfo id vol stb lf
  = CgIdInfo { cg_id = id
             , cg_elems = [mkCgIdElemInfo rep vol stb lf]
             }
  where
    rep = case idCgRep id of [rep] -> rep; _ -> panic "mkCgIdInfo"

mkCgIdElemInfo :: CgRep -> VolatileLoc -> StableLoc -> LambdaFormInfo -> CgIdElemInfo
mkCgIdElemInfo rep vol stb lf
  = CgIdElemInfo { cg_vol = vol, cg_stb = stb
                 , cg_lf = lf, cg_rep = rep, cg_tag = funTagLFInfo lf }
  where 

data VolatileLoc        -- These locations die across a call
  = NoVolatileLoc
  | RegLoc      CmmReg             -- In one of the registers (global or local)
  | VirHpLoc    VirtualHpOffset  -- Hp+offset (address of closure)
  | VirNodeLoc  ByteOff            -- Cts of offset indirect from Node
                                   -- ie *(Node+offset).
                                   -- NB. Byte offset, because we subtract R1's
                                   -- tag from the offset.

-- Used only for Id with a guaranteed-unary CgRep
mkTaggedCgIdElemInfo :: Id -> VolatileLoc -> StableLoc -> LambdaFormInfo -> DataCon
                     -> CgIdElemInfo
mkTaggedCgIdElemInfo id vol stb lf con
  = CgIdElemInfo { cg_rep = rep, cg_vol = vol, cg_stb = stb
                 , cg_lf = lf, cg_tag = tagForCon con }
  where rep = case idCgRep id of [rep] -> rep; _ -> panic "mkTaggedCgIdElemInfo"
\end{code}

@StableLoc@ encodes where an Id can be found, used by
the @CgBindings@ environment in @CgBindery@.

\begin{code}
data StableLoc
  = NoStableLoc

  | VirStkLoc   VirtualSpOffset         -- The thing is held in this
                                        -- stack slot

  | VirStkLNE   VirtualSpOffset         -- A let-no-escape thing; the
                                        -- value is this stack pointer
                                        -- (as opposed to the contents of the slot)

  | StableLoc   CmmExpr

instance PlatformOutputable CgIdInfo where
  pprPlatform platform (CgIdInfo id elems)
    = ppr id <+> ptext (sLit "-->") <+> vcat (map (pprPlatform platform) elems)

instance PlatformOutputable CgIdElemInfo where
  pprPlatform platform (CgIdElemInfo _ vol stb _ _)
    -- TODO, pretty pring the tag info
    = vcat [ppr vol, pprPlatform platform stb]

instance Outputable VolatileLoc where
  ppr NoVolatileLoc = empty
  ppr (RegLoc r)     = ptext (sLit "reg") <+> ppr r
  ppr (VirHpLoc v)   = ptext (sLit "vh")  <+> ppr v
  ppr (VirNodeLoc v) = ptext (sLit "vn")  <+> ppr v

instance PlatformOutputable StableLoc where
  pprPlatform _        NoStableLoc   = empty
  pprPlatform _        (VirStkLoc v) = ptext (sLit "vs")    <+> ppr v
  pprPlatform _        (VirStkLNE v) = ptext (sLit "lne")   <+> ppr v
  pprPlatform platform (StableLoc a) = ptext (sLit "amode") <+> pprPlatform platform a
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Bindery-idInfo]{Manipulating IdInfo}
%*                                                                      *
%************************************************************************

\begin{code}
stableIdInfo :: Id -> CmmExpr -> LambdaFormInfo -> CgIdInfo
stableIdInfo id amode   lf_info = mkCgIdInfo id NoVolatileLoc (StableLoc amode) lf_info

heapIdInfo :: Id -> VirtualHpOffset -> LambdaFormInfo -> CgIdInfo
heapIdInfo id offset    lf_info = mkCgIdInfo id (VirHpLoc offset) NoStableLoc lf_info

letNoEscapeIdInfo :: Id -> VirtualSpOffset -> LambdaFormInfo -> CgIdInfo
letNoEscapeIdInfo id sp lf_info = mkCgIdInfo id NoVolatileLoc (VirStkLNE sp) lf_info

stackIdElemInfo :: CgRep -> VirtualSpOffset -> LambdaFormInfo -> CgIdElemInfo
stackIdElemInfo rep sp  lf_info = mkCgIdElemInfo rep NoVolatileLoc (VirStkLoc sp) lf_info

nodeIdElemInfo :: CgRep -> VirtualHpOffset -> LambdaFormInfo -> CgIdElemInfo
nodeIdElemInfo rep offset lf_info = mkCgIdElemInfo rep (VirNodeLoc (wORD_SIZE*offset)) NoStableLoc lf_info

untagNodeIdElemInfo :: CgRep -> VirtualHpOffset -> LambdaFormInfo -> Int -> CgIdElemInfo
untagNodeIdElemInfo rep offset lf_info tag
  = mkCgIdElemInfo rep (VirNodeLoc (wORD_SIZE*offset - tag)) NoStableLoc lf_info

regIdElemInfo :: CgRep -> CmmReg -> LambdaFormInfo -> CgIdElemInfo
regIdElemInfo rep reg lf_info = mkCgIdElemInfo rep (RegLoc reg) NoStableLoc lf_info

taggedStableIdInfo :: Id -> CmmExpr -> LambdaFormInfo -> DataCon -> CgIdInfo
taggedStableIdInfo id amode lf_info con
  = CgIdInfo id [mkTaggedCgIdElemInfo id NoVolatileLoc (StableLoc amode) lf_info con]

taggedHeapIdInfo :: Id -> VirtualHpOffset -> LambdaFormInfo -> DataCon -> CgIdInfo
taggedHeapIdInfo id offset lf_info con
  = CgIdInfo id [mkTaggedCgIdElemInfo id (VirHpLoc offset) NoStableLoc lf_info con]


idInfoToAmodes :: CgIdInfo -> FCode [CmmExpr]
idInfoToAmodes = mapM idElemInfoToAmode . cg_elems

idElemInfoToAmode :: CgIdElemInfo -> FCode CmmExpr
idElemInfoToAmode info
  = case cg_vol info of {
      RegLoc reg        -> returnFC (CmmReg reg) ;
      VirNodeLoc nd_off -> returnFC (CmmLoad (cmmOffsetB (CmmReg nodeReg) nd_off)
                                             mach_rep) ;
      VirHpLoc hp_off   -> do { off <- getHpRelOffset hp_off
                              ; return $! maybeTag off };
      NoVolatileLoc -> 

    case cg_stb info of
      StableLoc amode  -> returnFC $! maybeTag amode
      VirStkLoc sp_off -> do { sp_rel <- getSpRelOffset sp_off
                             ; return (CmmLoad sp_rel mach_rep) }

      VirStkLNE sp_off -> getSpRelOffset sp_off

      NoStableLoc -> panic "idInfoToAmode: no loc"
    }
  where
    mach_rep = argMachRep (cg_rep info)

    maybeTag amode  -- add the tag, if we have one
      | tag == 0   = amode
      | otherwise  = cmmOffsetB amode tag
      where tag = cg_tag info

cgIdInfoId :: CgIdInfo -> Id
cgIdInfoId = cg_id 

cgIdInfoElems :: CgIdInfo -> [CgIdElemInfo]
cgIdInfoElems = cg_elems

cgIdInfoSingleElem :: String -> CgIdInfo -> CgIdElemInfo
cgIdInfoSingleElem _ (CgIdInfo { cg_elems = [elem] }) = elem
cgIdInfoSingleElem msg _ = panic $ "cgIdInfoSingleElem: " ++ msg

cgIdElemInfoLF :: CgIdElemInfo -> LambdaFormInfo
cgIdElemInfoLF = cg_lf

cgIdElemInfoArgRep :: CgIdElemInfo -> CgRep
cgIdElemInfoArgRep = cg_rep

maybeLetNoEscape :: CgIdElemInfo -> Maybe VirtualSpOffset
maybeLetNoEscape (CgIdElemInfo { cg_stb = VirStkLNE sp_off }) = Just sp_off
maybeLetNoEscape _                                            = Nothing
\end{code}

%************************************************************************
%*                                                                      *
\subsection[CgMonad-bindery]{Monad things for fiddling with @CgBindings@}
%*                                                                      *
%************************************************************************

There are three basic routines, for adding (@addBindC@), modifying
(@modifyBindC@) and looking up (@getCgIdInfo@) bindings.

A @Id@ is bound to a @(VolatileLoc, StableLoc)@ triple.
The name should not already be bound. (nice ASSERT, eh?)

Note [CgIdInfo knot]
~~~~~~~~~~~~~~~~~~~~

We can't be too strict in the CgIdInfo, because in e.g. letrecs the CgIdInfo
is knot-tied. A loop I build in practice was
  cgExpr LetRec -> cgRhs StgRhsCon -> buildDynCon'
from code like (let xs = (:) y xs in xs) because we fixpoint the CgIdInfo for
xs and buildDynCon' is strict in the length of the CgIdElemInfo list.

To work around this we try to be yield the length of the CgIdInfo element list
lazily by lazily zipping it with the idCgReps.
\begin{code}
addBindC :: Id -> CgIdInfo -> Code
addBindC name stuff_to_bind = do
        binds <- getBinds
        setBinds $ extendVarEnv binds name stuff_to_bind

addBindsC :: [(Id, CgIdInfo)] -> Code
addBindsC new_bindings = do
        binds <- getBinds
        let new_binds = foldl (\ binds (name,info) -> extendVarEnv binds name info)
                              binds
                              new_bindings
        setBinds new_binds

modifyBindC :: Id -> (CgIdInfo -> CgIdInfo) -> Code
modifyBindC name mangle_fn = do
        binds <- getBinds
        setBinds $ modifyVarEnv mangle_fn binds name

-- See: Note [CgIdInfo knot]
etaCgIdInfo :: Id -> CgIdInfo -> CgIdInfo
etaCgIdInfo id ~(CgIdInfo { cg_id = lazy_id, cg_elems = elems })
  = CgIdInfo { cg_id = lazy_id
             , cg_elems = zipLazyWith (showPpr (id, idCgRep id, length elems)) (\_ elem -> elem) (idCgRep id) elems }

-- Note eta-expansion of CgIdInfo: 
getCgIdInfo :: Id -> FCode CgIdInfo
getCgIdInfo id
  = liftM (etaCgIdInfo id) $
    do  {       -- Try local bindings first
        ; local_binds  <- getBinds
        ; case lookupVarEnv local_binds id of {
            Just info -> return info ;
            Nothing   -> do

        {       -- Try top-level bindings
          static_binds <- getStaticBinds
        ; case lookupVarEnv static_binds id of {
            Just info -> return info ;
            Nothing   ->

                -- Should be imported; make up a CgIdInfo for it
        let 
            name = idName id
        in
        if isExternalName name then do
            let ext_lbl = CmmLit (CmmLabel (mkClosureLabel name $ idCafInfo id))
            return $ case mkLFImported id of
                Nothing      -> CgIdInfo id []
                Just lf_info -> stableIdInfo id ext_lbl lf_info
        else
        -- Bug  
        cgLookupPanic id
        }}}}
    
                        
cgLookupPanic :: Id -> FCode a
cgLookupPanic id
  = do  static_binds <- getStaticBinds
        local_binds <- getBinds
--      srt <- getSRTLabel
        pprPanic "cgLookupPanic (probably invalid Core; try -dcore-lint)"
                (vcat [ppr id,
                ptext (sLit "static binds for:"),
                vcat [ ppr (cg_id info) | info <- varEnvElts static_binds ],
                ptext (sLit "local binds for:"),
                vcat [ ppr (cg_id info) | info <- varEnvElts local_binds ]
--              ptext (sLit "SRT label") <+> pprCLabel srt
              ])
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Bindery-nuke-volatile]{Nuking volatile bindings}
%*                                                                      *
%************************************************************************

We sometimes want to nuke all the volatile bindings; we must be sure
we don't leave any (NoVolatile, NoStable) binds around...

\begin{code}
nukeVolatileBinds :: CgBindings -> CgBindings
nukeVolatileBinds binds
  = mkVarEnv (foldr (\info acc -> case keep_if_stable (cg_elems info) of Just infos -> (cg_id info, info { cg_elems = infos }) : acc; Nothing -> acc) [] (varEnvElts binds))
  where
    has_no_stable_loc (CgIdElemInfo { cg_stb = NoStableLoc }) = True
    has_no_stable_loc _                                       = False

    keep_if_stable infos
      | any has_no_stable_loc infos
      = ASSERT(all has_no_stable_loc infos)
        Nothing
      | otherwise
      = Just (map (\info -> info { cg_vol = NoVolatileLoc }) infos)
\end{code}


%************************************************************************
%*                                                                      *
\subsection[lookup-interface]{Interface functions to looking up bindings}
%*                                                                      *
%************************************************************************

\begin{code}
getVolatilesCAddrModes :: Id -> FCode [Maybe (CgRep, CmmExpr)]
getVolatilesCAddrModes id
  = do  { info <- getCgIdInfo id
        ; forM (cg_elems info) $ \elem_info -> case cg_stb elem_info of
                NoStableLoc -> liftM (\expr -> Just (cg_rep elem_info, expr))
                                     (idElemInfoToAmode elem_info)
                _           -> return Nothing }
\end{code}

@getVolatileRegs@ gets a set of live variables, and returns a list of
all registers on which these variables depend. These are the regs
which must be saved and restored across any C calls. If a variable is
both in a volatile location (depending on a register) {\em and} a
stable one (notably, on the stack), we modify the current bindings to
forget the volatile one.

\begin{code}
getVolatileRegs :: StgLiveVars -> FCode [GlobalReg]
getVolatileRegs vars = do
  do    { stuff <- mapFCs snaffle_it (varSetElems vars)
        ; returnFC $ concat stuff }
  where
    snaffle_it var = do
        { info <- getCgIdInfo var 
        ; let (vol_regs, elems') = unzip $ flip map (cg_elems info) $ \elem_info -> 
                let    -- commoned-up code...
                    consider_reg reg
                       =       -- We assume that all regs can die across C calls
                               -- We leave it to the save-macros to decide which
                               -- regs *really* need to be saved.
                         case cg_stb elem_info of
                               NoStableLoc -> (Just reg, elem_info) -- got one!
                                -- has both volatile & stable locations;
                                -- force it to rely on the stable location
                               _ -> (Nothing, elem_info { cg_vol = NoVolatileLoc })
                in case cg_vol elem_info of
                     RegLoc (CmmGlobal reg) -> consider_reg reg
                     VirNodeLoc _           -> consider_reg node
                     _                      -> (Nothing, elem_info)  -- Local registers
        ; modifyBindC var (const info { cg_elems = elems' })
        ; return (catMaybes vol_regs)
        }

getArgAmodes :: StgArg -> FCode [(CgRep, CmmExpr)]
getArgAmodes (StgVarArg var) 
  = do  { info <- getCgIdInfo var
        ; forM (cg_elems info) $ \elem_info -> do
                amode <- idElemInfoToAmode elem_info
                return (cg_rep elem_info, amode) }
getArgAmodes (StgLitArg lit) 
  = do  { cmm_lit <- cgLit lit
        ; return $ zipEqual "getArgAmodes" (typeCgRep (literalType lit)) [CmmLit cmm_lit] }
getArgAmodes (StgTypeArg _) = return []
\end{code}

%************************************************************************
%*                                                                      *
\subsection[binding-and-rebinding-interface]{Interface functions for binding and re-binding names}
%*                                                                      *
%************************************************************************

\begin{code}
bindArgsToRegOrStack :: [(Id, [Either GlobalReg VirtualSpOffset])] -> Code
bindArgsToRegOrStack = mapCs bind
  where
    bind (id, ei_reg_offs) = addBindC id $ CgIdInfo id $
        zipWith3Equal "bindArgsToRegOrStack"
                      (\rep lf_info ei_reg_off -> case ei_reg_off of
                        Left reg  -> regIdElemInfo   rep (CmmGlobal reg) lf_info
                        Right off -> stackIdElemInfo rep off             lf_info)
                      (idCgRep id) (mkLFArgument (idType id)) ei_reg_offs

bindNewToNode :: Id -> [(VirtualHpOffset, LambdaFormInfo)] -> Code
bindNewToNode id offset_lf_infos
  = addBindC id (CgIdInfo id $ zipWithEqual "bindNewToNode" (\rep (offset, lf_info) -> nodeIdElemInfo rep offset lf_info) (idCgRep id) offset_lf_infos)

-- NB: the tag is for the *node*, not the thing we load from it, so it is shared amongst elements
bindNewToUntagNode :: Id -> [(VirtualHpOffset, LambdaFormInfo)] -> Int -> Code
bindNewToUntagNode id offset_lf_infos tag
  = addBindC id (CgIdInfo id $ zipWithEqual "bindNewToUntagNode" (\rep (offset, lf_info) -> untagNodeIdElemInfo rep offset lf_info tag) (idCgRep id) offset_lf_infos)

idRegs :: Id -> FCode [LocalReg]
idRegs id = do
    us <- newUniqSupply
    let cg_reps = idCgRep id
        temp_regs = zipWith LocalReg (getUnique id : uniqsFromSupply us) (map argMachRep cg_reps)
    return temp_regs

-- Create a new temporary whose unique is that in the id,
-- bind the id to it, and return the addressing mode for the
-- temporary.
bindNewToTemp :: Id -> FCode [LocalReg]
bindNewToTemp id
  = do  temp_regs <- idRegs id
        bindToRegs id temp_regs
        return temp_regs

bindToRegs :: Id -> [LocalReg] -> FCode ()
bindToRegs id temp_regs
  = addBindC id $ CgIdInfo id $ zipWith3Equal "bindNewToTemp" (\rep temp_reg lf_info -> regIdElemInfo rep (CmmLocal temp_reg) lf_info) (idCgRep id) temp_regs lf_infos
  where
    lf_infos = mkLFArgument (idType id)  -- Always used of things we
                                         -- know nothing about

bindNewToReg :: Id -> [(CmmReg, LambdaFormInfo)] -> Code
bindNewToReg name regs_lf_infos
  = addBindC name (CgIdInfo name elem_infos)
  where
    elem_infos = zipWithEqual "bindNewToReg" (\rep (reg, lf_info) -> regIdElemInfo rep reg lf_info)
                                             (idCgRep name) regs_lf_infos

rebindToStack :: Id -> [Maybe VirtualSpOffset] -> Code
rebindToStack name offsets
  = modifyBindC name replace_stable_fn
  where
    replace_stable_fn info = info { cg_elems = zipWithEqual "rebindToStack" (\elem_info mb_offset -> case mb_offset of Just offset -> elem_info { cg_stb = VirStkLoc offset }; Nothing -> elem_info) (cg_elems info) offsets }
\end{code}

%************************************************************************
%*                                                                      *
\subsection[CgMonad-deadslots]{Finding dead stack slots}
%*                                                                      *
%************************************************************************

nukeDeadBindings does the following:

      - Removes all bindings from the environment other than those
        for variables in the argument to nukeDeadBindings.
      - Collects any stack slots so freed, and returns them to the  stack free
        list.
      - Moves the virtual stack pointer to point to the topmost used
        stack locations.

You can have multi-word slots on the stack (where a Double# used to
be, for instance); if dead, such a slot will be reported as *several*
offsets (one per word).

Probably *naughty* to look inside monad...

\begin{code}
nukeDeadBindings :: StgLiveVars  -- All the *live* variables
                 -> Code
nukeDeadBindings live_vars = do
        binds <- getBinds
        let (dead_stk_slots, bs') =
                dead_slots live_vars 
                        [] [] []
                        [ (cg_id b, b) | b <- varEnvElts binds ]
        setBinds $ mkVarEnv bs'
        freeStackSlots dead_stk_slots
\end{code}

Several boring auxiliary functions to do the dirty work.

Note that some stack slots can be mentioned in *more than one* CgIdInfo.
This commonly happens where the stack slots for the case binders of an
unboxed tuple case are a subset of the stack slots for the unboxed tuple case binder.

\begin{code}
dead_slots :: StgLiveVars
           -> [(Id,CgIdInfo)]
           -> [VirtualSpOffset]
           -> [VirtualSpOffset]
           -> [(Id,CgIdInfo)]
           -> ([VirtualSpOffset], [(Id,CgIdInfo)])

-- dead_slots carries accumulating parameters for
--      filtered bindings, possibly-dead slots, live slots
dead_slots _ fbs ds ls []
  = (ds \\ ls, reverse fbs) -- Finished; rm the dups, if any

dead_slots live_vars fbs ds ls ((v,i):bs)
  | v `elementOfUniqSet` live_vars
    = dead_slots live_vars ((v,i):fbs) ds (infoLiveSlots i ++ ls) bs
          -- Live, so don't record it in dead slots
          -- Instead keep it in the filtered bindings

  | otherwise
    = dead_slots live_vars fbs (infoLiveSlots i ++ ds) ls bs

infoLiveSlots :: CgIdInfo -> [WordOff]
infoLiveSlots i = [free | elem_i <- cg_elems i
                        , VirStkLoc offset <- [cg_stb elem_i]
                        , let size = cgRepSizeW (cg_rep elem_i) :: WordOff
                        , size > 0
                        , free <- [offset-size+1 .. offset]]

getLiveStackSlots :: FCode [VirtualSpOffset]
-- Return the offsets of slots in stack containig live pointers
getLiveStackSlots 
  = do  { binds <- getBinds
        ; return [off | info <- varEnvElts binds
                      , CgIdElemInfo { cg_stb = VirStkLoc off
                                     , cg_rep = rep } <- cg_elems info
                      , isFollowableArg rep] }

getLiveStackBindings :: FCode [(VirtualSpOffset, CgRep)]
getLiveStackBindings
  = do { binds <- getBinds
       ; return [(off, rep) |
                 info <- varEnvElts binds,
                 elem_info <- cg_elems info,
                 CgIdElemInfo { cg_stb = VirStkLoc off,
                                cg_rep = rep} <- [elem_info],
                 isFollowableArg rep] }
\end{code}

