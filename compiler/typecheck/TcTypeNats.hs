module TcTypeNats (typeNatTyThings, TcBuiltInSynFamily(..)) where

import Type
import Pair
import TcType     ( TcType )
import TyCon      ( TyCon, SynTyConRhs(..), mkSynTyCon, TyConParent(..)  )
import Coercion   ( Role(..) )
import TcRnTypes  ( Xi )
import TcEvidence ( mkTcAxiomRuleCo, TcCoercion )
import CoAxiom    ( CoAxiomRule(..) )
import Name       ( Name, mkWiredInName, BuiltInSyntax(..) )
import OccName    ( mkOccName, tcName )
import Unique     ( mkAxiomRuleUnique )
import TysWiredIn ( typeNatKind )
import TysPrim    ( tyVarList, mkArrowKinds )
import PrelNames  ( gHC_PRIM
                  , typeNatAddTyFamName
                  , typeNatMulTyFamName
                  , typeNatExpTyFamName
                  )
import FamInst(TcBuiltInSynFamily(..),trivialBuiltInFamily)

typeNatTyThings :: [TyThing]

typeNatTyThings = typeNatTyCons ++ typeNatAxioms


{-------------------------------------------------------------------------------
Built-in type constructors for functions on type-lelve nats
-}

typeNatTyCons :: [TyThing]
typeNatTyCons = map ATyCon
  [ typeNatAddTyCon
  , typeNatMulTyCon
  , typeNatExpTyCon
  ]

typeNatAddTyCon :: TyCon
typeNatAddTyCon = mkTypeNatFunTyCon2 typeNatAddTyFamName
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamAdd
    , sfInteractTop   = interactTopAdd
    , sfInteractInert = sfInteractInert trivialBuiltInFamily
    }

typeNatMulTyCon :: TyCon
typeNatMulTyCon = mkTypeNatFunTyCon2 typeNatMulTyFamName
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamMul
    , sfInteractTop   = interactTopMul
    , sfInteractInert = sfInteractInert trivialBuiltInFamily
    }

typeNatExpTyCon :: TyCon
typeNatExpTyCon = mkTypeNatFunTyCon2 typeNatExpTyFamName
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamExp
    , sfInteractTop   = interactTopExp
    , sfInteractInert = sfInteractInert trivialBuiltInFamily
    }

-- Make a binary built-in constructor of kind: Nat -> Nat -> Nat
mkTypeNatFunTyCon2 :: Name -> TcBuiltInSynFamily -> TyCon
mkTypeNatFunTyCon2 op tcb =
  mkSynTyCon op
    (mkArrowKinds [ typeNatKind, typeNatKind ] typeNatKind)
    (take 2 $ tyVarList typeNatKind)
    [Nominal,Nominal]
    (BuiltInSynFamTyCon tcb)
    NoParentTyCon




{-------------------------------------------------------------------------------
Built-in rules axioms
-------------------------------------------------------------------------------}

axAddDefKey
  , axMulDefKey
  , axExpDefKey
  , axAdd0LKey
  , axAdd0RKey
  , axMul0LKey
  , axMul0RKey
  , axMul1LKey
  , axMul1RKey
  , axExp1LKey
  , axExp0RKey
  , axExp1RKey
  :: Int
axAddDefKey = 0
axMulDefKey = 1
axExpDefKey = 2
axAdd0LKey  = 3
axAdd0RKey  = 4
axMul0LKey  = 5
axMul0RKey  = 6
axMul1LKey  = 7
axMul1RKey  = 8
axExp1LKey  = 9
axExp0RKey  = 10
axExp1RKey  = 11


axAddDef
  , axMulDef
  , axExpDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R

  :: CoAxiomRule
axAddDef = mkBinAxiom axAddDefKey "AddDef" typeNatAddTyCon (+) -- s + t ~ (s+t)
axMulDef = mkBinAxiom axMulDefKey "MulDef" typeNatMulTyCon (*) -- s * t ~ (s*t)
axExpDef = mkBinAxiom axExpDefKey "ExpDef" typeNatExpTyCon (^) -- s ^ t ~ (s^t)
axAdd0L  = mkAxiom1   axAdd0LKey  "Add0L" $ \t -> (num 0 .+. t) === t
axAdd0R  = mkAxiom1   axAdd0RKey  "Add0R" $ \t -> (t .+. num 0) === t
axMul0L  = mkAxiom1   axMul0LKey  "Mul0L" $ \t -> (num 0 .*. t) === num 0
axMul0R  = mkAxiom1   axMul0RKey  "Mul0R" $ \t -> (t .*. num 0) === num 0
axMul1L  = mkAxiom1   axMul1LKey  "Mul1L" $ \t -> (num 1 .*. t) === t
axMul1R  = mkAxiom1   axMul1RKey  "Mul1R" $ \t -> (t .*. num 1) === t
axExp1L  = mkAxiom1   axExp1LKey  "Exp1L" $ \t -> (num 1 .^. t) === num 1
axExp0R  = mkAxiom1   axExp0RKey  "Exp0R" $ \t -> (t .^. num 0) === num 1
axExp1R  = mkAxiom1   axExp1RKey  "Exp1R" $ \t -> (t .^. num 1) === t

typeNatAxioms :: [TyThing]
typeNatAxioms = map ACoAxiomRule
  [ axAddDef
  , axMulDef
  , axExpDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R
  ]




{-------------------------------------------------------------------------------
Various utilities for making axioms and types
-------------------------------------------------------------------------------}

(.+.) :: Type -> Type -> Type
s .+. t = mkTyConApp typeNatAddTyCon [s,t]

(.*.) :: Type -> Type -> Type
s .*. t = mkTyConApp typeNatMulTyCon [s,t]

(.^.) :: Type -> Type -> Type
s .^. t = mkTyConApp typeNatExpTyCon [s,t]

(===) :: Type -> Type -> Pair Type
x === y = Pair x y

num :: Integer -> Type
num = mkNumLitTy


mkAxName :: Int -> String -> (Name -> CoAxiomRule) -> CoAxiomRule
mkAxName n s r = thing
  where
  thing = r name

  -- XXX: I'm not sure that we should be using the type name space here
  name  = mkWiredInName gHC_PRIM (mkOccName tcName s) (mkAxiomRuleUnique n)
                          (ACoAxiomRule thing) BuiltInSyntax

-- For the definitional axioms
mkBinAxiom :: Int -> String -> TyCon ->
              (Integer -> Integer -> Integer) -> CoAxiomRule
mkBinAxiom key str tc f = mkAxName key str $ \name ->
  CoAxiomRule
    { coaxrName      = name
    , coaxrTypeArity = 2
    , coaxrAsmpArity = 0
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s,t],[]) -> do x <- isNumLitTy s
                           y <- isNumLitTy t
                           return (Pair (mkTyConApp tc [s,t])
                                        (mkNumLitTy (f x y)))
          _ -> Nothing
    }

mkAxiom1 :: Int -> String -> (Type -> Pair Type) -> CoAxiomRule
mkAxiom1 key str f = mkAxName key str $ \name ->
  CoAxiomRule
    { coaxrName      = name
    , coaxrTypeArity = 1
    , coaxrAsmpArity = 0
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s],[]) -> return (f s)
          _        -> Nothing
    }

{-------------------------------------------------------------------------------
Evaluation
-------------------------------------------------------------------------------}

matchFamAdd :: [Type] -> Maybe (TcCoercion, TcType)
matchFamAdd [s,t]
  | Just 0 <- mbX = Just (mkTcAxiomRuleCo axAdd0L [t] [], t)
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axAdd0R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axAddDef [s,t] [], mkNumLitTy (x + y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamAdd _ = Nothing

matchFamMul :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamMul [s,t]
  | Just 0 <- mbX = Just (mkTcAxiomRuleCo axMul0L [t] [], num 0)
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axMul0R [s] [], num 0)
  | Just 1 <- mbX = Just (mkTcAxiomRuleCo axMul1L [t] [], t)
  | Just 1 <- mbY = Just (mkTcAxiomRuleCo axMul1R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axMulDef [s,t] [], mkNumLitTy (x * y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamMul _ = Nothing

matchFamExp :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamExp [s,t]
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axExp0R [s] [], num 1)
  | Just 1 <- mbX = Just (mkTcAxiomRuleCo axExp1L [t] [], num 1)
  | Just 1 <- mbY = Just (mkTcAxiomRuleCo axExp1R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axExpDef [s,t] [], mkNumLitTy (x ^ y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamExp _ = Nothing


{-------------------------------------------------------------------------------
Interact with axioms
-------------------------------------------------------------------------------}

interactTopAdd :: [Xi] -> Xi -> [Pair Type]
interactTopAdd [s,t] r
  | Just 0 <- mbZ = [ s === num 0, t === num 0 ]
  | Just x <- mbX, Just z <- mbZ, Just y <- minus z x = [t === num y]
  | Just y <- mbY, Just z <- mbZ, Just x <- minus z y = [s === num x]
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopAdd _ _ = []

interactTopMul :: [Xi] -> Xi -> [Pair Type]
interactTopMul [s,t] r
  | Just 1 <- mbZ = [ s === num 1, t === num 1 ]
  | Just x <- mbX, Just z <- mbZ, Just y <- divide z x = [t === num y]
  | Just y <- mbY, Just z <- mbZ, Just x <- divide z y = [s === num x]
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopMul _ _ = []

interactTopExp :: [Xi] -> Xi -> [Pair Type]
interactTopExp [s,t] r
  | Just 0 <- mbZ = [ s === num 0 ]
  | Just x <- mbX, Just z <- mbZ, Just y <- logExact  z x = [t === num y]
  | Just y <- mbY, Just z <- mbZ, Just x <- rootExact z y = [s === num x]
  where
  mbX = isNumLitTy s
  mbY = isNumLitTy t
  mbZ = isNumLitTy r
interactTopExp _ _ = []





{- -----------------------------------------------------------------------------
These inverse functions are used for simplifying propositions using
concrete natural numbers.
----------------------------------------------------------------------------- -}

-- | Subtract two natural numbers.
minus :: Integer -> Integer -> Maybe Integer
minus x y = if x >= y then Just (x - y) else Nothing

-- | Compute the exact logarithm of a natural number.
-- The logarithm base is the second argument.
logExact :: Integer -> Integer -> Maybe Integer
logExact x y = do (z,True) <- genLog x y
                  return z


-- | Divide two natural numbers.
divide :: Integer -> Integer -> Maybe Integer
divide _ 0  = Nothing
divide x y  = case divMod x y of
                (a,0) -> Just a
                _     -> Nothing

-- | Compute the exact root of a natural number.
-- The second argument specifies which root we are computing.
rootExact :: Integer -> Integer -> Maybe Integer
rootExact x y = do (z,True) <- genRoot x y
                   return z



{- | Compute the the n-th root of a natural number, rounded down to
the closest natural number.  The boolean indicates if the result
is exact (i.e., True means no rounding was done, False means rounded down).
The second argument specifies which root we are computing. -}
genRoot :: Integer -> Integer -> Maybe (Integer, Bool)
genRoot _  0    = Nothing
genRoot x0 1    = Just (x0, True)
genRoot x0 root = Just (search 0 (x0+1))
  where
  search from to = let x = from + div (to - from) 2
                       a = x ^ root
                   in case compare a x0 of
                        EQ              -> (x, True)
                        LT | x /= from  -> search x to
                           | otherwise  -> (from, False)
                        GT | x /= to    -> search from x
                           | otherwise  -> (from, False)

{- | Compute the logarithm of a number in the given base, rounded down to the
closest integer.  The boolean indicates if we the result is exact
(i.e., True means no rounding happened, False means we rounded down).
The logarithm base is the second argument. -}
genLog :: Integer -> Integer -> Maybe (Integer, Bool)
genLog x 0    = if x == 1 then Just (0, True) else Nothing
genLog _ 1    = Nothing
genLog 0 _    = Nothing
genLog x base = Just (exactLoop 0 x)
  where
  exactLoop s i
    | i == 1     = (s,True)
    | i < base   = (s,False)
    | otherwise  =
        let s1 = s + 1
        in s1 `seq` case divMod i base of
                      (j,r)
                        | r == 0    -> exactLoop s1 j
                        | otherwise -> (underLoop s1 j, False)

  underLoop s i
    | i < base  = s
    | otherwise = let s1 = s + 1 in s1 `seq` underLoop s1 (div i base)







