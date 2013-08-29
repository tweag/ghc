module TcTypeNatsRules where

-- From other GHC locations
import Var      ( TyVar )
import Type     ( Type,  mkTyVarTy, mkNumLitTy, mkTyConApp
                , CoAxiomRule, Eqn, co_axr_rule, co_axr_tynum2_rule
                , TyThing(ACoAxiomRule)
                )
import PrelNames ( gHC_PRIM )
import TysPrim  ( tyVarList)
import TysWiredIn ( typeNatAddTyCon
                  , typeNatMulTyCon
                  , typeNatExpTyCon
                  , typeNatLeqTyCon
                  , typeNatSubTyCon
                  , boolKind, trueTy, falseTy
                  , nat1Kind, zeroTy
                  , fromNat1TyCon
                  , typeNatKind
                  )

import Name     ( Name, mkWiredInName, BuiltInSyntax(..) )
import OccName  ( mkOccName, tcName )
import Unique   ( mkAxiomRuleUnique )


typeNatRuleThings :: [TyThing]
typeNatRuleThings = map ACoAxiomRule $
  [ axAddDef, axMulDef, axExpDef, axLeqDef ]
    ++ [ leq0, leqRefl, leqTrans, leqAsym ]
    ++ bRules
    ++ map snd impRules
    ++ map snd widenRules
    ++ iffRules



--------------------------------------------------------------------------------
mkAxName :: Int -> String -> (Name -> CoAxiomRule) -> CoAxiomRule
mkAxName n s r = thing
  where
  thing = r name

  -- XXX: I'm not sure that we should be using the type name space here
  name  = mkWiredInName gHC_PRIM (mkOccName tcName s) (mkAxiomRuleUnique n)
                          (ACoAxiomRule thing) BuiltInSyntax

mkAx :: Int -> String -> [TyVar] -> [Eqn] -> Eqn -> CoAxiomRule
mkAx u s as es e = mkAxName u s $ \n -> co_axr_rule n as es e

mkDef :: Int -> String -> (Integer -> Integer -> Eqn) -> CoAxiomRule
mkDef u s f = mkAxName u s $ \n -> co_axr_tynum2_rule n f


mkAdd :: Type -> Type -> Type
mkAdd a b = mkTyConApp typeNatAddTyCon [a,b]

mkMul :: Type -> Type -> Type
mkMul a b = mkTyConApp typeNatMulTyCon [a,b]

mkExp :: Type -> Type -> Type
mkExp a b = mkTyConApp typeNatExpTyCon [a,b]

mkLeq :: Type -> Type -> Type
mkLeq a b = mkTyConApp typeNatLeqTyCon [a,b]

mkSub :: Type -> Type -> Type
mkSub a b = mkTyConApp typeNatSubTyCon [a,b]

mkFromNat1 :: Type -> Type
mkFromNat1 a = mkTyConApp fromNat1TyCon [a]

natVars :: [TyVar]
natVars = tyVarList typeNatKind

boolVars :: [TyVar]
boolVars = tyVarList boolKind

nat1Vars :: [TyVar]
nat1Vars = tyVarList nat1Kind

mkBoolLiTy :: Bool -> Type
mkBoolLiTy b = if b then trueTy else falseTy

-- Just some sugar to make the rules a bit more readable
(===) :: Type -> Type -> Eqn
x === y = (x,y)

(<==) :: Type -> Type -> Eqn
x <== y = (mkLeq x y, trueTy)


--------------------------------------------------------------------------------
axAddDef :: CoAxiomRule
axAddDef = mkDef 0 "AddDef" $ \a b ->
             mkAdd (mkNumLitTy a) (mkNumLitTy b) === mkNumLitTy (a + b)

axMulDef :: CoAxiomRule
axMulDef = mkDef 1 "MulDef" $ \a b ->
             mkMul (mkNumLitTy a) (mkNumLitTy b) === mkNumLitTy (a * b)

axExpDef :: CoAxiomRule
axExpDef = mkDef 2 "ExpDef" $ \a b ->
             mkExp (mkNumLitTy a) (mkNumLitTy b) === mkNumLitTy (a ^ b)

axLeqDef :: CoAxiomRule
axLeqDef = mkDef 3 "LeqDef" $ \a b ->
             mkLeq (mkNumLitTy a) (mkNumLitTy b) === mkBoolLiTy (a <= b)


bRules :: [CoAxiomRule]
bRules =
  [ bRule 10 "Add0L" (mkAdd n0 a === a)
  , bRule 11 "Add0R" (mkAdd a n0 === a)

  , bRule 12 "Mul0L" (mkMul n0 a === n0)
  , bRule 13 "Mul0R" (mkMul a n0 === n0)
  , bRule 14 "Mul1L" (mkMul n1 a === a)
  , bRule 15 "Mul1R" (mkMul a n1 === a)

  -- TnExp0L:  see iffRules
  , bRule 17 "TnExp0R" (mkExp a n0 === n1)
  , bRule 18 "TnExp1L" (mkExp n1 a === n1)
  , bRule 19 "TnExp1R" (mkExp a n1 === a)
  ]
  where
  bRule s n = mkAx s n (take 1 natVars) []
  a : _     = map mkTyVarTy natVars
  n0        = mkNumLitTy 0
  n1        = mkNumLitTy 1


leq0 :: CoAxiomRule
leq0 = mkAx 20 "Leq0" (take 1 natVars) [] (mkLeq (mkNumLitTy 0) a  === trueTy)
  where a : _ = map mkTyVarTy natVars

leqRefl :: CoAxiomRule
leqRefl = mkAx 21 "LeqRefl" (take 1 natVars) [] (mkLeq a a  === trueTy)
  where a : _ = map mkTyVarTy natVars

leqTrans :: CoAxiomRule
leqTrans = mkAx 42 "LeqTrans" (take 3 natVars) [ a <== b, b <== c ] (a <== c)
  where a : b : c : _ = map mkTyVarTy natVars

leqAsym :: CoAxiomRule
leqAsym = mkAx 36 "LeqASym" (take 2 natVars) [ a <== b, b <== a ] (a === b)
  where a : b : _ = map mkTyVarTy natVars



impRules :: [(Bool,CoAxiomRule)]
impRules =
  [ (True, mkAx 30 "AddCancelL" (take 4 natVars)
            [ mkAdd a b === d, mkAdd a c === d ] (b === c))

  , (True, mkAx 31 "AddCancelR" (take 4 natVars)
            [ mkAdd a c === d, mkAdd b c === d ] (a === b))

  , (True, mkAx 32 "MulCancelL" (take 4 natVars)
            [ n1 <== a, mkMul a b === d, mkMul a c === d ] (b === c))

  , (True, mkAx 33 "MulCancelR" (take 4 natVars)
            [ n1 <== c, mkMul a c === d, mkMul b c === d ] (a === b))

  , (True, mkAx 34 "ExpCancelL" (take 4 natVars)
            [ n2 <== a, mkExp a b === d, mkExp a c === d ] (b === c))

  , (True, mkAx 35 "ExpCancelR" (take 4 natVars)
            [ n1 <== c, mkExp a c === d, mkExp b c === d ] (a === b))

  , (True, leqAsym)
  ]

  where
  a : b : c : d : _ = map mkTyVarTy natVars
  n1 = mkNumLitTy 1
  n2 = mkNumLitTy 2


widenRules :: [(Bool,CoAxiomRule)]
widenRules =
  [ (True, mkAx 40 "AddComm" (take 3 natVars)
            [ mkAdd a b === c ] (mkAdd b a === c))

  , (True, mkAx 41 "MulComm" (take 3 natVars)
            [ mkMul a b === c ] (mkMul b a === c))

  , (False, mkAx 43 "AddAssoc1" (take 6 natVars)
      [ mkAdd a b === x, mkAdd b c === y, mkAdd a y === z ] (mkAdd x c === z))

  , (False, mkAx 44 "AddAssoc2" (take 6 natVars)
      [ mkAdd a b === x, mkAdd b c === y, mkAdd x c === z ] (mkAdd a y === z))

  , (False, mkAx 45 "MulAssoc1" (take 6 natVars)
      [ mkMul a b === x, mkMul b c === y, mkMul a y === z ] (mkMul x c === z))

  , (False, mkAx 46 "MulAssoc2" (take 6 natVars)
      [ mkMul a b === x, mkMul b c === y, mkMul x c === z ] (mkMul a y === z))


  -- XXX: Some simple interactions between operators and ordering.
  -- A proper interval analysis would do better.
  , (True, mkAx 50 "LeqAdd1" (take 3 natVars)
      [ mkAdd a b === c ] (a <== c))

  , (True, mkAx 51 "LeqAdd2" (take 3 natVars)
      [ mkAdd a b === c ] (b <== c))

  , (True, mkAx 52 "LeqMul1" (take 3 natVars)
      [ n1 <== b, mkMul a b === c ] (a <== c))

  , (True, mkAx 53 "LeqMul2" (take 3 natVars)
      [ n1 <== a, mkMul a b === c ] (b <== c))

  , (True, mkAx 54 "LeqExp1" (take 3 natVars)
      [ n1 <== b, mkExp a b === c ] (a <== c))

  , (True, mkAx 54 "LeqExp2" (take 3 natVars)
      [ n2 <== a, mkExp a b === c ] (b <== c))


  -- Improvements related to `FromNat1`
  , (True, mkAx 60 "FromNat1_inj" (take 2 nat1Vars ++ take 1 (drop 2 natVars))
      [ mkFromNat1 a1 === c, mkFromNat1 b1 === c] (a1 === b1))

  {- This follows from the injectivity rule, when interacted with the
     definition for `FromNat1`.  We have the explicit rule here
     because this is the only function that has explicit user-defined
     equations and so we have no support for interacting with
     existing equations.   This could be removed if GHC knows about
     injective type families. -}
  , (True, mkAx 61 "FromNat1Zero" (take 1 nat1Vars)
      [ mkFromNat1 a1 === n0 ] (a === zeroTy))

  {- We also want:
      forall (a :: Nat1) (b :: Nat).i
      exists (c :: Nat1)
        (FromNat1 a ~ b, 1 <= b) => (a ~ Succ c)

    Currently this is implemented with some hand-written code in TcTypeNats.hs
  -}

  -- Desugaring rules for `iff` rules, forward direction.
  , (True, mkAx 66 "SubE" (take 3 natVars)
      [ mkSub c b === a ] (mkAdd a b === c))

  ]
  where
  a  : b  : c : x : y : z : _ = map mkTyVarTy natVars
  a1 : b1 : _ = map mkTyVarTy nat1Vars
  n0 = mkNumLitTy 0
  n1 = mkNumLitTy 1
  n2 = mkNumLitTy 2

iffRules :: [CoAxiomRule]
iffRules =
  [ mkAx 65 "SubI"    (take 3 natVars) [ mkAdd a b === c ] (mkSub c b === a)
  , mkAx 67 "TnExp0L" (take 1 natVars) [ n1 <== a        ] (mkExp n0 a === n0)
  ]
  where
  a : b : c : _ = map mkTyVarTy natVars
  n0 = mkNumLitTy 0
  n1 = mkNumLitTy 1




