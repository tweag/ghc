-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: the binding environment
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module StgCmmEnv (
	CgIdInfo,

	cgIdInfoId, cgIdInfoElems, cgIdInfoSingleElem,
        cgIdElemInfoLF,

	litIdInfo, lneIdInfo, regIdInfo,
	idInfoToAmodes, idElemInfoToAmode,

	addBindC, addBindsC,

	bindArgsToRegs, bindToReg, rebindToReg,
	bindArgToReg, idToReg,
	addArgReps, getArgAmodes,
	getCgIdInfo, 
	maybeLetNoEscape, 
    ) where

#include "HsVersions.h"

import StgCmmMonad
import StgCmmUtils
import StgCmmClosure

import CLabel

import BlockId
import CmmExpr
import CmmUtils
import MkGraph (CmmAGraph, mkAssign, (<*>))
import UniqSupply (uniqsFromSupply)
import FastString
import Type (PrimRep)
import Id
import VarEnv
import Control.Monad
import Name
import StgSyn
import DynFlags
import Outputable
import Util

-------------------------------------
--	Manipulating CgIdInfo
-------------------------------------

mkCgIdElemInfo :: LambdaFormInfo -> CmmExpr -> CgIdElemInfo
mkCgIdElemInfo lf expr
  = CgIdElemInfo { cg_lf = lf
                 , cg_loc = CmmLoc expr, 
	           cg_tag = lfDynTag lf }

mkCgIdInfo :: Id -> LambdaFormInfo -> CmmExpr -> CgIdInfo
mkCgIdInfo id lf expr
  = CgIdInfo { cg_id = id
             , cg_elems = [mkCgIdElemInfo lf expr]
             }

-- Used for building info for external names (which are always lifted)
-- and closures/constructors (which are always represented as a single pointer)
litIdInfo :: Id -> LambdaFormInfo -> CmmLit -> CgIdInfo
litIdInfo id lf lit
  = CgIdInfo { cg_id = id
             , cg_elems = [CgIdElemInfo { cg_lf = lf
                                        , cg_loc = CmmLoc (addDynTag (CmmLit lit) tag)
                                        , cg_tag = tag }]
             }
  where
    tag = lfDynTag lf

lneIdInfo :: Id -> [LocalReg] -> CgIdInfo
lneIdInfo id regs 
  = CgIdInfo { cg_id    = id
             , cg_elems = [CgIdElemInfo { cg_lf = lf
                                        , cg_loc = LneLoc blk_id regs
                                        , cg_tag = lfDynTag lf }]
             }
  where
    lf     = mkLFLetNoEscape
    blk_id = mkBlockId (idUnique id)

-- Because the register may be spilled to the stack in untagged form, we
-- modify the initialization code 'init' to immediately tag the
-- register, and store a plain register in the CgIdInfo.  We allocate
-- a new register in order to keep single-assignment and help out the
-- inliner. -- EZY
regIdInfo :: Id -> LambdaFormInfo -> LocalReg -> CmmAGraph -> FCode (CgIdInfo, CmmAGraph)
regIdInfo id lf_info reg init
  = do { reg' <- newTemp (localRegType reg)
       ; let init' = init <*> mkAssign (CmmLocal reg') 
                                       (addDynTag (CmmReg (CmmLocal reg)) 
                                                  (lfDynTag lf_info))
       ; return (mkCgIdInfo id lf_info (CmmReg (CmmLocal reg')), init') }

idElemInfoToAmode :: CgIdElemInfo -> CmmExpr
-- Returns a CmmExpr for the *tagged* pointer
idElemInfoToAmode (CgIdElemInfo { cg_loc = CmmLoc e }) = e
idElemInfoToAmode _cg_info
  = panic "idElemInfoToAmode: LneLoc"

idInfoToAmodes :: CgIdInfo -> [CmmExpr]
idInfoToAmodes = map idElemInfoToAmode . cg_elems

addDynTag :: CmmExpr -> DynTag -> CmmExpr
-- A tag adds a byte offset to the pointer
addDynTag expr tag = cmmOffsetB expr tag

cgIdInfoId :: CgIdInfo -> Id
cgIdInfoId = cg_id 

cgIdInfoElems :: CgIdInfo -> [CgIdElemInfo]
cgIdInfoElems = cg_elems

-- Used for where the caller knows there will only be one alternative (commonly
-- because it knows the info is for a thunk, closure or some data)
cgIdInfoSingleElem :: CgIdInfo -> CgIdElemInfo
cgIdInfoSingleElem (CgIdInfo { cg_elems = [elem_info] }) = elem_info
cgIdInfoSingleElem _ = panic "cgIdInfoSingleElem"

cgIdElemInfoLF :: CgIdElemInfo -> LambdaFormInfo
cgIdElemInfoLF = cg_lf

maybeLetNoEscape :: CgIdElemInfo -> Maybe (BlockId, [LocalReg])
maybeLetNoEscape (CgIdElemInfo { cg_loc = LneLoc blk_id args}) = Just (blk_id, args)
maybeLetNoEscape _other         			       = Nothing



---------------------------------------------------------
--	The binding environment
-- 
-- There are three basic routines, for adding (addBindC), 
-- modifying(modifyBindC) and looking up (getCgIdInfo) bindings.
---------------------------------------------------------

-- Note [CgIdInfo knot]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- We can't be too strict in the CgIdInfo, because in e.g. letrecs the CgIdInfo
-- is knot-tied. A loop I build in practice was
--   cgExpr LetRec -> cgRhs StgRhsCon -> buildDynCon'
-- from code like (let xs = (:) y xs in xs) because we fixpoint the CgIdInfo for
-- xs and buildDynCon' is strict in the length of the CgIdElemInfo list.
--
-- To work around this we try to be yield the length of the CgIdInfo element list
-- lazily by lazily zipping it with the idCgReps.

addBindC :: Id -> CgIdInfo -> FCode ()
addBindC name stuff_to_bind = do
	binds <- getBinds
	setBinds $ extendVarEnv binds name stuff_to_bind

addBindsC :: [CgIdInfo] -> FCode ()
addBindsC new_bindings = do
	binds <- getBinds
	let new_binds = foldl (\ binds info -> extendVarEnv binds (cg_id info) info)
			      binds
			      new_bindings
	setBinds new_binds

-- See: Note [CgIdInfo knot]
etaCgIdInfo :: Id -> CgIdInfo -> CgIdInfo
etaCgIdInfo id ~(CgIdInfo { cg_id = lazy_id, cg_elems = elems })
  = CgIdInfo { cg_id = lazy_id
             , cg_elems = zipLazyWith (showPpr (id, idPrimRep id, length elems)) (\_ elem -> elem) (idPrimRep id) elems }

getCgIdInfo :: Id -> FCode CgIdInfo
getCgIdInfo id
  = liftM (etaCgIdInfo id) $
    do	{ 	-- Try local bindings first
	; local_binds  <- getBinds
	; case lookupVarEnv local_binds id of {
	    Just info -> return info ;
	    Nothing   -> do

	{ 	-- Try top-level bindings
	  static_binds <- getStaticBinds
	; case lookupVarEnv static_binds id of {
	    Just info -> return info ;
	    Nothing   ->

		-- Should be imported; make up a CgIdInfo for it
	let 
	    name = idName id
	in
	if isExternalName name then do
	    { let ext_lbl = CmmLabel (mkClosureLabel name $ idCafInfo id)
        ; return $ case mkLFImported id of
              Just lf_info -> litIdInfo id lf_info ext_lbl
              Nothing      -> CgIdInfo id [] }

	else
	-- Bug	
	cgLookupPanic id
	}}}}
    
cgLookupPanic :: Id -> FCode a
cgLookupPanic id
  = do	dflags <- getDynFlags
      	static_binds <- getStaticBinds
	local_binds <- getBinds
	srt <- getSRTLabel
	pprPanic "StgCmmEnv: variable not found"
		(vcat [ppr id,
		ptext (sLit "static binds for:"),
		vcat [ ppr (cg_id info) | info <- varEnvElts static_binds ],
		ptext (sLit "local binds for:"),
		vcat [ ppr (cg_id info) | info <- varEnvElts local_binds ],
	        ptext (sLit "SRT label") <+> pprCLabel (targetPlatform dflags) srt
	      ])


--------------------
getArgAmodes :: StgArg -> FCode [CmmExpr]
getArgAmodes (StgVarArg var)  =
  do { info  <- getCgIdInfo var; return (idInfoToAmodes info) }
getArgAmodes (StgLitArg lit)  = liftM (return . CmmLit) $ cgLit lit
getArgAmodes (StgTypeArg _)   = return []

addArgReps :: StgArg -> FCode [(PrimRep, CmmExpr)]
addArgReps arg = do
    exprs <- getArgAmodes arg
    return (zipEqual "addArgReps" (argPrimRep arg) exprs)

------------------------------------------------------------------------
--	Interface functions for binding and re-binding names
------------------------------------------------------------------------

bindToReg :: Id -> [(LocalReg, LambdaFormInfo)] -> FCode ()
-- Bind an Id to a fresh LocalReg
bindToReg id regs_lf_infos
  = do	{ addBindC id (CgIdInfo { cg_id = id
                                , cg_elems = map (\(reg, lf_info) -> mkCgIdElemInfo lf_info (CmmReg (CmmLocal reg))) regs_lf_infos }) }

rebindToReg :: Id -> [LocalReg] -> FCode ()
-- Like bindToReg, but the Id is already in scope, so 
-- get its LF info from the envt
rebindToReg id regs
  = do	{ info <- getCgIdInfo id
	; bindToReg id (zipEqual "rebindToReg" regs (map cgIdElemInfoLF (cg_elems info))) }

bindArgToReg :: Id -> [LocalReg] -> FCode ()
bindArgToReg id regs = bindToReg id (zipEqual "bindArgToReg" regs (mkLFArgument (idType id)))

bindArgsToRegs :: [(Id, [LocalReg])] -> FCode ()
bindArgsToRegs args = mapM_ (uncurry bindArgToReg) args

idToReg :: Id -> FCode [LocalReg]
-- Make a register from an Id, typically a function argument,
-- free variable, or case binder
--
-- We re-use the Unique from the Id to make it easier to see what is going on
--
-- By now the Ids should be uniquely named; else one would worry
-- about accidental collision 
idToReg id = do
    us <- newUniqSupply
    return $ zipWith LocalReg (idUnique id : uniqsFromSupply us) (map primRepCmmType (idPrimRep id))
