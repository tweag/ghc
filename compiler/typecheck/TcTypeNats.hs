module TcTypeNats
  ( typeNatTyCons
  , typeNatCoAxiomRules
  , TcBuiltInSynFamily(..)
  , LinArithResult(..)
  , decideLinArith
  , isLinArithTyCon
  ) where

import Type
import Pair
import TcType     ( TcType )
import TyCon      ( TyCon, SynTyConRhs(..), mkSynTyCon, TyConParent(..)  )
import Coercion   ( Role(..) )
import TcRnTypes  ( Xi, Ct(..), CtLoc, ctFlavour, ctPred, CtFlavour(..)
                  , CtEvidence(..) )
import TcEvidence ( mkTcAxiomRuleCo, TcCoercion, EvTerm(..) )
import CoAxiom    ( CoAxiomRule(..) )
import Name       ( Name, BuiltInSyntax(..), nameUnique, nameOccName )
import OccName    ( occNameString )
import Var        ( tyVarName, tyVarKind )
import TysWiredIn ( typeNatKind, mkWiredInTyConName
                  , promotedBoolTyCon
                  , promotedFalseDataCon, promotedTrueDataCon
                  , typeNatKindCon
                  )
import TysPrim    ( tyVarList, mkArrowKinds )
import PrelNames  ( gHC_TYPELITS
                  , typeNatAddTyFamNameKey
                  , typeNatMulTyFamNameKey
                  , typeNatExpTyFamNameKey
                  , typeNatLeqTyFamNameKey
                  , typeNatSubTyFamNameKey
                  )
import FamInst    ( TcBuiltInSynFamily(..) )
import FastString ( FastString, fsLit )
import Outputable ( ppr, panic, pprPanic )
import UniqSet    ( foldUniqSet )
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.Maybe ( isJust )
import Data.List  ( tails, genericReplicate )
import Data.List  ( genericReplicate )
import System.Cmd ( system )


isLinArithTyCon :: TyCon -> [Xi] -> Bool
isLinArithTyCon tc ts =
  tc `elem` [ typeNatAddTyCon, typeNatLeqTyCon, typeNatSubTyCon ] ||
  (tc == typeNatMulTyCon && any (isJust . isNumLitTy) ts)


{-------------------------------------------------------------------------------
Built-in type constructors for functions on type-lelve nats
-}

typeNatTyCons :: [TyCon]
typeNatTyCons =
  [ typeNatAddTyCon
  , typeNatMulTyCon
  , typeNatExpTyCon
  , typeNatLeqTyCon
  , typeNatSubTyCon
  ]

typeNatAddTyCon :: TyCon
typeNatAddTyCon = mkTypeNatFunTyCon2 name
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamAdd
    , sfInteractTop   = interactTopAdd
    , sfInteractInert = interactInertAdd
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "+")
            typeNatAddTyFamNameKey typeNatAddTyCon

typeNatSubTyCon :: TyCon
typeNatSubTyCon = mkTypeNatFunTyCon2 name
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamSub
    , sfInteractTop   = interactTopSub
    , sfInteractInert = interactInertSub
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "-")
            typeNatSubTyFamNameKey typeNatSubTyCon

typeNatMulTyCon :: TyCon
typeNatMulTyCon = mkTypeNatFunTyCon2 name
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamMul
    , sfInteractTop   = interactTopMul
    , sfInteractInert = interactInertMul
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "*")
            typeNatMulTyFamNameKey typeNatMulTyCon

typeNatExpTyCon :: TyCon
typeNatExpTyCon = mkTypeNatFunTyCon2 name
  TcBuiltInSynFamily
    { sfMatchFam      = matchFamExp
    , sfInteractTop   = interactTopExp
    , sfInteractInert = interactInertExp
    }
  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "^")
                typeNatExpTyFamNameKey typeNatExpTyCon

typeNatLeqTyCon :: TyCon
typeNatLeqTyCon =
  mkSynTyCon name
    (mkArrowKinds [ typeNatKind, typeNatKind ] boolKind)
    (take 2 $ tyVarList typeNatKind)
    [Nominal,Nominal]
    (BuiltInSynFamTyCon ops)
    NoParentTyCon

  where
  name = mkWiredInTyConName UserSyntax gHC_TYPELITS (fsLit "<=?")
                typeNatLeqTyFamNameKey typeNatLeqTyCon
  ops = TcBuiltInSynFamily
    { sfMatchFam      = matchFamLeq
    , sfInteractTop   = interactTopLeq
    , sfInteractInert = interactInertLeq
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

-- If you add additional rules, please remember to add them to
-- `typeNatCoAxiomRules` also.
axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R
  , axLeqRefl
  , axLeq0L
  , axSubDef
  , axSub0R
  :: CoAxiomRule

axAddDef = mkBinAxiom "AddDef" typeNatAddTyCon $
              \x y -> Just $ num (x + y)

axMulDef = mkBinAxiom "MulDef" typeNatMulTyCon $
              \x y -> Just $ num (x * y)

axExpDef = mkBinAxiom "ExpDef" typeNatExpTyCon $
              \x y -> Just $ num (x ^ y)

axLeqDef = mkBinAxiom "LeqDef" typeNatLeqTyCon $
              \x y -> Just $ bool (x <= y)

axSubDef = mkBinAxiom "SubDef" typeNatSubTyCon $
              \x y -> fmap num (minus x y)

axAdd0L     = mkAxiom1 "Add0L"    $ \t -> (num 0 .+. t) === t
axAdd0R     = mkAxiom1 "Add0R"    $ \t -> (t .+. num 0) === t
axSub0R     = mkAxiom1 "Sub0R"    $ \t -> (t .-. num 0) === t
axMul0L     = mkAxiom1 "Mul0L"    $ \t -> (num 0 .*. t) === num 0
axMul0R     = mkAxiom1 "Mul0R"    $ \t -> (t .*. num 0) === num 0
axMul1L     = mkAxiom1 "Mul1L"    $ \t -> (num 1 .*. t) === t
axMul1R     = mkAxiom1 "Mul1R"    $ \t -> (t .*. num 1) === t
axExp1L     = mkAxiom1 "Exp1L"    $ \t -> (num 1 .^. t) === num 1
axExp0R     = mkAxiom1 "Exp0R"    $ \t -> (t .^. num 0) === num 1
axExp1R     = mkAxiom1 "Exp1R"    $ \t -> (t .^. num 1) === t
axLeqRefl   = mkAxiom1 "LeqRefl"  $ \t -> (t <== t) === bool True
axLeq0L     = mkAxiom1 "Leq0L"    $ \t -> (num 0 <== t) === bool True

typeNatCoAxiomRules :: Map.Map FastString CoAxiomRule
typeNatCoAxiomRules = Map.fromList $ map (\x -> (coaxrName x, x))
  [ axAddDef
  , axMulDef
  , axExpDef
  , axLeqDef
  , axAdd0L
  , axAdd0R
  , axMul0L
  , axMul0R
  , axMul1L
  , axMul1R
  , axExp1L
  , axExp0R
  , axExp1R
  , axLeqRefl
  , axLeq0L
  , axSubDef
  , smtSolver
  ]

decisionProcedure :: String -> CoAxiomRule
decisionProcedure name =
  CoAxiomRule
    { coaxrName      = fsLit name
    , coaxrTypeArity = 2
    , coaxrAsmpRoles = []
    , coaxrRole      = Nominal
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s,t],[]) -> return (s === t)
          _          -> Nothing
    }

smtSolver :: CoAxiomRule
smtSolver = decisionProcedure "smt"

evBySMT :: Type -> Type -> EvTerm
evBySMT t1 t2 = EvCoercion $ mkTcAxiomRuleCo smtSolver [t1,t2] []




{-------------------------------------------------------------------------------
Various utilities for making axioms and types
-------------------------------------------------------------------------------}

(.+.) :: Type -> Type -> Type
s .+. t = mkTyConApp typeNatAddTyCon [s,t]

(.-.) :: Type -> Type -> Type
s .-. t = mkTyConApp typeNatSubTyCon [s,t]

(.*.) :: Type -> Type -> Type
s .*. t = mkTyConApp typeNatMulTyCon [s,t]

(.^.) :: Type -> Type -> Type
s .^. t = mkTyConApp typeNatExpTyCon [s,t]

(<==) :: Type -> Type -> Type
s <== t = mkTyConApp typeNatLeqTyCon [s,t]

(===) :: Type -> Type -> Pair Type
x === y = Pair x y

num :: Integer -> Type
num = mkNumLitTy

boolKind :: Kind
boolKind = mkTyConApp promotedBoolTyCon []

bool :: Bool -> Type
bool b = if b then mkTyConApp promotedTrueDataCon []
              else mkTyConApp promotedFalseDataCon []

isBoolLitTy :: Type -> Maybe Bool
isBoolLitTy tc =
  do (tc,[]) <- splitTyConApp_maybe tc
     case () of
       _ | tc == promotedFalseDataCon -> return False
         | tc == promotedTrueDataCon  -> return True
         | otherwise                   -> Nothing

known :: (Integer -> Bool) -> TcType -> Bool
known p x = case isNumLitTy x of
              Just a  -> p a
              Nothing -> False




-- For the definitional axioms
mkBinAxiom :: String -> TyCon ->
              (Integer -> Integer -> Maybe Type) -> CoAxiomRule
mkBinAxiom str tc f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrTypeArity = 2
    , coaxrAsmpRoles = []
    , coaxrRole      = Nominal
    , coaxrProves    = \ts cs ->
        case (ts,cs) of
          ([s,t],[]) -> do x <- isNumLitTy s
                           y <- isNumLitTy t
                           z <- f x y
                           return (mkTyConApp tc [s,t] === z)
          _ -> Nothing
    }

mkAxiom1 :: String -> (Type -> Pair Type) -> CoAxiomRule
mkAxiom1 str f =
  CoAxiomRule
    { coaxrName      = fsLit str
    , coaxrTypeArity = 1
    , coaxrAsmpRoles = []
    , coaxrRole      = Nominal
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
    Just (mkTcAxiomRuleCo axAddDef [s,t] [], num (x + y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamAdd _ = Nothing

matchFamSub :: [Type] -> Maybe (TcCoercion, TcType)
matchFamSub [s,t]
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axSub0R [s] [], s)
  | Just x <- mbX, Just y <- mbY, Just z <- minus x y =
    Just (mkTcAxiomRuleCo axSubDef [s,t] [], num z)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamSub _ = Nothing

matchFamMul :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamMul [s,t]
  | Just 0 <- mbX = Just (mkTcAxiomRuleCo axMul0L [t] [], num 0)
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axMul0R [s] [], num 0)
  | Just 1 <- mbX = Just (mkTcAxiomRuleCo axMul1L [t] [], t)
  | Just 1 <- mbY = Just (mkTcAxiomRuleCo axMul1R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axMulDef [s,t] [], num (x * y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamMul _ = Nothing

matchFamExp :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamExp [s,t]
  | Just 0 <- mbY = Just (mkTcAxiomRuleCo axExp0R [s] [], num 1)
  | Just 1 <- mbX = Just (mkTcAxiomRuleCo axExp1L [t] [], num 1)
  | Just 1 <- mbY = Just (mkTcAxiomRuleCo axExp1R [s] [], s)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axExpDef [s,t] [], num (x ^ y))
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamExp _ = Nothing

matchFamLeq :: [Xi] -> Maybe (TcCoercion, Xi)
matchFamLeq [s,t]
  | Just 0 <- mbX = Just (mkTcAxiomRuleCo axLeq0L [t] [], bool True)
  | Just x <- mbX, Just y <- mbY =
    Just (mkTcAxiomRuleCo axLeqDef [s,t] [], bool (x <= y))
  | eqType s t  = Just (mkTcAxiomRuleCo axLeqRefl [s] [], bool True)
  where mbX = isNumLitTy s
        mbY = isNumLitTy t
matchFamLeq _ = Nothing

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

{- NOTE:
A simpler interaction here might be:

  `s - t ~ r` --> `t + r ~ s`

This would enable us to reuse all the code for addition.
Unfortunately, this works a little too well at the moment.
Consider the following example:

    0 - 5 ~ r --> 5 + r ~ 0 --> (5 = 0, r = 0)

This (correctly) spots that the constraint cannot be solved.

However, this may be a problem if the constraint did not
need to be solved in the first place!  Consider the following example:

f :: Proxy (If (5 <=? 0) (0 - 5) (5 - 0)) -> Proxy 5
f = id

Currently, GHC is strict while evaluating functions, so this does not
work, because even though the `If` should evaluate to `5 - 0`, we
also evaluate the "else" branch which generates the constraint `0 - 5 ~ r`,
which fails.

So, for the time being, we only add an improvement when the RHS is a constant,
which happens to work OK for the moment, although clearly we need to do
something more general.
-}
interactTopSub :: [Xi] -> Xi -> [Pair Type]
interactTopSub [s,t] r
  | Just z <- mbZ = [ s === (num z .+. t) ]
  where
  mbZ = isNumLitTy r
interactTopSub _ _ = []





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

interactTopLeq :: [Xi] -> Xi -> [Pair Type]
interactTopLeq [s,t] r
  | Just 0 <- mbY, Just True <- mbZ = [ s === num 0 ]
  where
  mbY = isNumLitTy t
  mbZ = isBoolLitTy r
interactTopLeq _ _ = []



{-------------------------------------------------------------------------------
Interaction with inerts
-------------------------------------------------------------------------------}

interactInertAdd :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertAdd [x1,y1] z1 [x2,y2] z2
  | sameZ && eqType x1 x2         = [ y1 === y2 ]
  | sameZ && eqType y1 y2         = [ x1 === x2 ]
  where sameZ = eqType z1 z2
interactInertAdd _ _ _ _ = []

interactInertSub :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertSub [x1,y1] z1 [x2,y2] z2
  | sameZ && eqType x1 x2         = [ y1 === y2 ]
  | sameZ && eqType y1 y2         = [ x1 === x2 ]
  where sameZ = eqType z1 z2
interactInertSub _ _ _ _ = []

interactInertMul :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertMul [x1,y1] z1 [x2,y2] z2
  | sameZ && known (/= 0) x1 && eqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (/= 0) y1 && eqType y1 y2 = [ x1 === x2 ]
  where sameZ   = eqType z1 z2

interactInertMul _ _ _ _ = []

interactInertExp :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertExp [x1,y1] z1 [x2,y2] z2
  | sameZ && known (> 1) x1 && eqType x1 x2 = [ y1 === y2 ]
  | sameZ && known (> 0) y1 && eqType y1 y2 = [ x1 === x2 ]
  where sameZ = eqType z1 z2

interactInertExp _ _ _ _ = []


interactInertLeq :: [Xi] -> Xi -> [Xi] -> Xi -> [Pair Type]
interactInertLeq [x1,y1] z1 [x2,y2] z2
  | bothTrue && eqType x1 y2 && eqType y1 x2 = [ x1 === y1 ]
  | bothTrue && eqType y1 x2                 = [ (x1 <== y2) === bool True ]
  | bothTrue && eqType y2 x1                 = [ (x2 <== y1) === bool True ]
  where bothTrue = isJust $ do True <- isBoolLitTy z1
                               True <- isBoolLitTy z2
                               return ()

interactInertLeq _ _ _ _ = []




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



{- -----------------------------------------------------------------------------
Calling an external decision procedure
----------------------------------------------------------------------------- -}

data LinArithResult = SolvedWanted EvTerm
                    | IgnoreGiven
                    | Impossible
                    | Progress [(TyVar, Xi)] [Ct] [Ct] [(Ct, EvTerm)]
                        -- Args:
                        -- 1) turn into wanted and add to work list,
                        -- 2) stick in work list, and
                        -- 3) add to inert set
                        -- 4) discharged, set evidence

data Op   = Add | Sub | Mul | Leq | Eq | Implies | And
           deriving Show

data Term = Op Term Op Term
          | Num Integer
          | Bool Bool
          | Var String
          | Not Term

termToSMT :: Term -> Expr
termToSMT term =
  case term of
    Op t1 op t2 -> opToSMT op (termToSMT t1) (termToSMT t2)
    Num x       -> fromInteger x
    Bool x      -> if x then true else false
    Not t       -> eNot (termToSMT t)
    Var x       -> app x []

opToSMT :: Op -> Expr -> Expr -> Expr
opToSMT op =
  case op of
    Add         -> (+)
    Sub         -> (-)
    Mul         -> (*)
    Leq         -> nLeq
    Eq          -> (====)
    Implies     -> (===>)
    And         -> eAnd

instance Show Term where
  showsPrec p v =
    case v of
      Num n -> shows n
      Var s -> showString s
      Not v -> showParen (p > 8) $ showString "!" . showsPrec 8 v
      Bool b -> shows b
      Op a o b -> showParen (p > pOp) $ showsPrec pL a . showString opStr
                                      . showsPrec pR b
        where
          (pOp, pL, pR, opStr) =
            case o of
              Add -> (6, 6, 7, " + ")
              Sub -> (6, 6, 7, " - ")
              Mul -> (7, 7, 8, " * ")
              Leq -> (4, 5, 5, " <= ")
              Eq  -> (4, 5, 5, " == ")
              Implies -> (1, 2, 1, " -> ")
              And -> (3, 4, 3, " & ")

tyConToOp :: TyCon -> Op
tyConToOp tc
  | tc == typeNatAddTyCon = Add
  | tc == typeNatSubTyCon = Sub
  | tc == typeNatMulTyCon = Mul
  | tc == typeNatLeqTyCon = Leq
  | otherwise             = pprPanic "tyConToOp" (ppr tc)

tyConToBool :: TyCon -> Maybe Bool
tyConToBool tc
  | tc == promotedTrueDataCon  = Just True
  | tc == promotedFalseDataCon = Just False
tyConToBool _ = Nothing

varName :: TyVar -> String
varName x = let n = tyVarName x
                u = nameUnique n
            in occNameString (nameOccName n) ++ "_" ++ show u

typeToTerm :: Type -> Term
typeToTerm ty
  | Just x <- getTyVar_maybe ty = Var (varName x)
  | Just x <- isNumLitTy ty = Num x
  | Just (tc,[]) <- splitTyConApp_maybe ty
  , Just x <- tyConToBool tc = Bool x
  | otherwise = pprPanic "typeToTerm" (ppr ty)

ctToTerm :: Ct -> Term
ctToTerm CFunEqCan { cc_fun = tc, cc_tyargs = [t1,t2], cc_rhs = xi } =
  Op (Op (typeToTerm t1) (tyConToOp tc) (typeToTerm t2)) Eq (typeToTerm xi)
ctToTerm ct = pprPanic "ctToTerm" (ppr ct)

termToType :: VarMap -> Term -> Xi
termToType vmap t =
  case t of
    Num n  -> num n
    Bool b -> bool b
    Var x  -> mkTyVarTy $ stringToTyVar vmap x
    _      -> panic ("termToType: " ++ show t)

stringToTyVar :: VarMap -> String -> TyVar
stringToTyVar vmap x =
  case Map.lookup x vmap of
    Just ty -> ty
    Nothing -> panic ("stringToTyVar: " ++ x)


decideLinArith :: [Ct] -> [Ct] -> Ct -> IO LinArithResult
decideLinArith givenCts wantedCts goalCt = decide
  where
  givens  = map ctToTerm givenCts
  wanteds = map ctToTerm wantedCts
  goal    = ctToTerm goalCt
  varMap  = getVarTys (goalCt : wantedCts ++ givenCts)
  flav    = ctFlavour goalCt

  bySMT   = uncurry evBySMT . getEqPredTys . ctPred

  noProgress = return (Progress [] [] (goalCt : wantedCts)) []

  assumptions
    | flav == Wanted = givens ++ wanteds
    | otherwise      = givens

  decide =
    do yes <- prove varMap (assumptions ==> goal)
       if yes
          then return (if flav == Given then IgnoreGiven
                                        else SolvedWanted (bySMT goalCt))
          else checkConsistent

  checkConsistent =
    do res <- sat varMap (ands $ goal : assumptions)
       case res of
         Unsat   -> return Impossible
         Unknown -> improve []
         Sat m   -> improve m

  improve model =
    do let candidates =
             [ (x, valueToTerm v) | (x, v) <- model ] ++
             [ (x, Var y) | (x,xv):xs <- tails (Map.toList varMap)
                          , (y,yv)    <- xs
                          , eqType (tyVarKind xv) (tyVarKind yv) ]
       eqs <- fmap concat $ mapM tryAssign candidates
       prune eqs

  tryAssign (x, t) =
    do yes <- prove varMap (goal : assumptions ==> Op (Var x) Eq t)
       if not yes
          then return []
          else return [(x, t)]

  prune eqs
    | flav == Given =
      do let newGiven = map (mkGivenCt varMap $ cc_loc goalCt) eqs
         return $ Progress [] (wantedCts ++ newGiven) [goalCt] []
    | otherwise =
      do let newWanted = [ (stringToTyVar varMap x, termToType varMap t)
                         | (x, t) <- eqs ]
         (newInerts, solved) <-
            simplifyInerts (zip wanteds wantedCts)
                           (goal : givens) [goalCt] []
         return $ Progress newWanted [] newInerts solved

  simplifyInerts [] _ done solved = return (done, solved)
  simplifyInerts ((p, ct):todo) assmps done solved =
    do yes <- prove varMap (map fst todo ++ assmps ==> p)
       if yes
          then simplifyInerts todo assmps done ((ct, bySMT ct):solved)
          else simplifyInerts todo (p:assmps) (ct:done) solved

mkGivenCt :: VarMap -> CtLoc -> (String, Term) -> Ct
mkGivenCt varMap loc (x, t) =
    CTyEqCan { cc_ev = ev, cc_loc = loc, cc_tyvar = tv, cc_rhs = rhs }
  where
    ev = CtGiven { ctev_pred = mkEqPred lhs rhs
                 , ctev_evtm = evBySMT lhs rhs }
    tv  = stringToTyVar varMap x
    lhs = mkTyVarTy tv
    rhs = termToType varMap t

getVarTys :: [Ct] -> VarMap
getVarTys = Map.fromList
          . map addName
          . foldUniqSet (:) []
          . tyVarsOfTypes
          . concatMap ctTypes
  where
  addName x = (varName x, x)

  ctTypes CFunEqCan { cc_tyargs = [t1,t2], cc_rhs = xi } = [t1,t2,xi]
  ctTypes ct = pprPanic "getVarTys" (ppr ct)

--------------------------------------------------------------------------------

data Result = Sat [(String, Value)]
            | Unsat
            | Unknown
              deriving (Show)

instance Read Result where
  readsPrec _ xs = do ("sat", xs1) <- lex xs
                      (vs,rest) <- readParen True getVals xs1
                      return (Sat vs, rest)
                ++ do ("unsat", _) <- lex xs
                      return (Unsat, [])
                ++ do ("unknown", _) <- lex xs
                      return (Unknown, [])
    where
    getVals xs@(')' : _) = [ ([], xs) ]
    getVals xs = do (x,cs)    <- getVal xs
                    (rest,ds) <- getVals cs
                    return (x : rest, ds)

    getVal = readParen True $ \xs -> do (name,rest) <- lex xs
                                        (val,rest1) <- reads rest
                                        return ((name,val), rest1)



data Value = VInt Integer | VBool Bool

instance Show Value where
  show (VInt n)  = show n
  show (VBool b) = show b

instance Read Value where
  readsPrec p s = [ (VInt n,  s')     | (n, s') <- readsPrec p s ] ++
                  [ (VBool True, s')  | ("true", s') <- lex s ] ++
                  [ (VBool False, s') | ("false", s') <- lex s ]

valueToTerm :: Value -> Term
valueToTerm (VInt n)  = Num n
valueToTerm (VBool b) = Bool b

type VarMap = Map String TyVar

sat :: VarMap -> Term -> IO Result
sat varMap t =
  do writeFile "ghc_input.smt" $ pp script
     system ("cvc4 --lang=smtlib2 -m ghc_input.smt > ghc_output.smt")
     txt <- readFile "ghc_output.smt"
     {-
     putStrLn (replicate 50 '-')
     print t
     putStrLn (replicate 50 '-')
     putStrLn txt
     putStrLn (replicate 50 '-')
     -}
     case reads txt of
       [(v,_)] -> return v
       _ -> panic $ "sat:\n" ++ txt
  where
  declareVar (x,tv) =
    case splitTyConApp_maybe (tyVarKind tv) of
      Just (tc,[])
        | tc == promotedBoolTyCon -> [ CmdDeclareFun (N x) [] tBool ]
        | tc == typeNatKindCon    -> [ CmdDeclareFun (N x) [] tInt
                                     , CmdAssert (nGeq (app x []) 0)
                                     ]

      -- XXX: Could we encounter a kind variable?
      _ -> pprPanic "sat" (ppr (tyVarKind tv))

  script =
    Script $ [ CmdSetLogic (N "QF_LIA") ]
          ++ concatMap declareVar (Map.toList varMap)
          ++ [ CmdAssert (termToSMT t)
             , CmdCheckSat
             , CmdGetValue [ app x [] | x <- Map.keys varMap ]
             ]


prove :: VarMap -> Term -> IO Bool
prove varMap p =
  do x <- sat varMap (Not p)
     case x of
       Unsat -> return True
       _     -> return False

ands :: [Term] -> Term
ands [] = Bool True
ands ps = foldr1 (\x y -> Op x And y) ps

infixr 1 ==>

(==>) :: [Term] -> Term -> Term
[] ==> q  = q
ps ==> q  = Op (ands ps) Implies q


--------------------------------------------------------------------------------




newtype SmtName  = N String
                deriving (Eq,Ord,Show)

data Ident    = I SmtName [Integer]
                deriving (Eq,Ord,Show)

data Quant    = Exists | Forall
                deriving (Eq,Ord,Show)

data Binder   = Bind { bindVar :: SmtName, bindSmtType :: SmtType }
                deriving (Eq,Ord,Show)

data Defn     = Defn { defVar :: SmtName, defExpr :: Expr }
                deriving (Eq,Ord,Show)

data Literal  = LitNum Integer
              | LitFrac Rational
              | LitStr String
                deriving (Eq,Ord,Show)

data SmtType     = TApp Ident [SmtType]
              | TVar SmtName
                deriving (Eq,Ord,Show)

data Expr     = Lit Literal
              | App Ident (Maybe SmtType) [Expr]
              | Quant Quant [Binder] Expr
              | Let [Defn] Expr
              | Annot Expr [Attr]
                deriving (Eq,Ord,Show)

data Attr     = Attr { attrName :: SmtName , attrVal :: Maybe AttrVal }
                deriving (Eq,Ord,Show)

type AttrVal  = Expr    -- A bit of an approximation....


data Option
  = OptPrintSuccess Bool
  | OptExpandDefinitions Bool
  | OptInteractiveMode Bool
  | OptProduceProofs Bool
  | OptProduceUnsatCores Bool
  | OptProduceModels Bool
  | OptProduceAssignments Bool
  | OptRegularOutputChannel String
  | OptDiagnosticOutputChannel String
  | OptRandomSeed Integer
  | OptVerbosity Integer
  | OptAttr Attr

data InfoFlag
  = InfoAllStatistics
  | InfoErrorBehavior
  | InfoName
  | InfoAuthors
  | InfoVersion
  | InfoStatus
  | InfoReasonUnknown
  | InfoAttr Attr

data Command
  = CmdSetLogic SmtName
  | CmdSetOption Option
  | CmdSetInfo Attr
  | CmdDeclareSmtType SmtName Integer
  | CmdDefineSmtType SmtName [SmtName] SmtType
  | CmdDeclareFun SmtName [SmtType] SmtType
  | CmdDefineFun SmtName [Binder] SmtType Expr
  | CmdPush Integer
  | CmdPop Integer
  | CmdAssert Expr
  | CmdCheckSat
  | CmdGetAssertions
  | CmdGetValue [Expr]
  | CmdGetProof
  | CmdGetUnsatCore
  | CmdGetInfo InfoFlag
  | CmdGetOption SmtName
  | CmdExit

newtype Script = Script [Command]


--------------------------------------------------------------------------------
-- To make it a bit simpler to write terms in the above AST
-- we provide some instances.  They are intended to be used only
-- for writing simple literals, and not any of the computational
-- operations associated with the classes.

{-
-- Strings
instance IsString SmtName      where fromString   = N
instance IsString Ident     where fromString x = I (fromString x) []
instance IsString SmtType      where fromString x = TApp (fromString x) []
instance IsString Expr      where fromString   = Lit . LitStr . fromString
-}

-- Integers

-- NOTE: Some of these might not mean anything, depending on the theory.
instance Num Expr where
  fromInteger x = Lit (LitNum x)
  x + y         = app "+"      [x,y]
  x - y         = app "-"      [x,y]
  x * y         = app "*"      [x,y]
  signum x      = app "signum" [x]
  abs x         = app "abs"    [x]


-- Fractional numbers
instance Fractional Expr where
  fromRational x  = Lit (LitFrac x)
  x / y           = app "/" [x,y]

app :: String -> [Expr] -> Expr
app f es = App (I (N f) []) Nothing es

--------------------------------------------------------------------------------
type Doc = String

x <+> y   = fsep [x,y]
(<>)      = (++)
nest _ x  = x
integer   = show
char x    = [x]
parens x  = "(" ++ x ++ ")"
fsep      = unwords
x $$ y    = x ++ "\n" ++ y
vcat      = unlines
text      = id
empty     = ""


class PP t where
  pp :: t -> Doc

instance PP Bool where
  pp True   = text "true"
  pp False  = text "false"

instance PP Integer where
  pp        = integer

ppString :: String -> Doc
ppString = text . show

instance PP SmtName where
  pp (N x) = text x

instance PP Ident where
  pp (I x []) = pp x
  pp (I x is) = parens (char '_' <+> pp x <+> fsep (map integer is))

instance PP Attr where
  pp (Attr x v) = char ':' <> pp x <+> maybe empty pp v

instance PP Quant where
  pp Forall = text "forall"
  pp Exists = text "exists"

instance PP Expr where
  pp expr =
    case expr of

      Lit l     -> pp l

      App c ty ts  ->
        case ts of
          [] -> ppFun
          _  -> parens (ppFun <+> fsep (map pp ts))

        where ppFun = case ty of
                        Nothing -> pp c
                        Just t  -> parens (text "as" <+> pp c <+> pp t)

      Quant q bs e ->
        case bs of
          [] -> pp e
          _  -> parens (pp q <+> parens (fsep (map pp bs)) $$ nest 2 (pp e))

      Let ds e ->
        case ds of
          [] -> pp e
          _  -> parens (text "let" <+> (parens (vcat (map pp ds)) $$ pp e))

      Annot e as ->
        case as of
          [] -> pp e
          _  -> parens (char '!' <+> pp e $$ nest 2 (vcat (map pp as)))


instance PP Binder where
  pp (Bind x t) = parens (pp x <+> pp t)

instance PP Defn where
  pp (Defn x e)   = parens (pp x <+> pp e)

instance PP SmtType where
  pp ty =
    case ty of
      TApp c ts ->
        case ts of
          [] -> pp c
          _  -> parens (pp c <+> fsep (map pp ts))
      TVar x -> pp x

instance PP Literal where
  pp lit =
    case lit of

      LitNum n -> integer n

      LitFrac x -> text (show (fromRational x :: Double))  -- XXX: Good enough?

      LitStr x -> ppString x



instance PP Option where
  pp opt =
    case opt of
      OptPrintSuccess b             -> std "print-success" b
      OptExpandDefinitions b        -> std "expand-definitions" b
      OptInteractiveMode b          -> std "interactive-mode" b
      OptProduceProofs b            -> std "produce-proofs" b
      OptProduceUnsatCores b        -> std "produce-unsat-cores" b
      OptProduceModels b            -> std "produce-models" b
      OptProduceAssignments b       -> std "produce-assignments" b
      OptRegularOutputChannel s     -> str "regular-output-channel" s
      OptDiagnosticOutputChannel s  -> str "diagnostic-output-channel" s
      OptRandomSeed n               -> std "random-seed" n
      OptVerbosity n                -> std "verbosity" n
      OptAttr a                     -> pp a

    where mk a b  = char ':' <> text a <+> b
          std a b = mk a (pp b)
          str a b = mk a (ppString b)

instance PP InfoFlag where
  pp info =
    case info of
      InfoAllStatistics -> mk "all-statistics"
      InfoErrorBehavior -> mk "error-behavior"
      InfoName          -> mk "name"
      InfoAuthors       -> mk "authors"
      InfoVersion       -> mk "version"
      InfoStatus        -> mk "status"
      InfoReasonUnknown -> mk "reason-unknown"
      InfoAttr a        -> pp a
    where mk x = char ':' <> text x

instance PP Command where
  pp cmd =
    case cmd of
      CmdSetLogic n     -> std "set-logic" n
      CmdSetOption o    -> std "set-options" o
      CmdSetInfo a      -> std "set-info" a
      CmdDeclareSmtType x n    -> mk "declare-sort" (pp x <+> integer n)
      CmdDefineSmtType x as t  -> fun "define-sort" x as (pp t)
      CmdDeclareFun x ts t  -> fun "declare-fun" x ts (pp t)
      CmdDefineFun x bs t e -> fun "define-fun" x bs (pp t $$ nest 2 (pp e))
      CmdPush n         -> std "push" n
      CmdPop n          -> std "pop" n
      CmdAssert e       -> std "assert" e
      CmdCheckSat       -> one "check-sat"
      CmdGetAssertions  -> one "get-assertions"
      CmdGetValue es    -> mk  "get-value" (parens (fsep (map pp es)))
      CmdGetProof       -> one "get-proof"
      CmdGetUnsatCore   -> one "get-unsat-core"
      CmdGetInfo i      -> std "get-info" i
      CmdGetOption n    -> std "get-option" n
      CmdExit           -> one "exit"
    where mk x d = parens (text x <+> d)
          one x   = mk x empty
          std x a = mk x (pp a)
          fun x y as d = mk x (pp y <+> parens (fsep (map pp as)) <+> d)

instance PP Script where
  pp (Script cs) = vcat (map pp cs)



tBool :: SmtType
tBool = TApp (I (N "Bool") []) []

true :: Expr
true = app "true" []

false :: Expr
false = app "false" []

eNot :: Expr -> Expr
eNot p = app "not" [p]

(===>) :: Expr -> Expr -> Expr
p ===> q = app "=>" [p,q]

eAnd :: Expr -> Expr -> Expr
eAnd p q = app "and" [p,q]

eOr :: Expr -> Expr -> Expr
eOr p q = app "or" [p,q]

eXor :: Expr -> Expr -> Expr
eXor p q = app "xor" [p,q]

(====) :: Expr -> Expr -> Expr
x ==== y = app "=" [x,y]

(=/=) :: Expr -> Expr -> Expr
x =/= y = app "distinct" [x,y]

ite :: Expr -> Expr -> Expr -> Expr
ite b x y = app "ite" [b,x,y]



tInt :: SmtType
tInt = TApp (I (N "Int") []) []

nNeg :: Expr -> Expr
nNeg x = app "-" [x]

nSub :: Expr -> Expr -> Expr
nSub x y = app "-" [x,y]

nAdd :: Expr -> Expr -> Expr
nAdd x y = app "+" [x,y]

nMul :: Expr -> Expr -> Expr
nMul x y = app "*" [x,y]

nDiv :: Expr -> Expr -> Expr
nDiv x y = app "div" [x,y]

nMod :: Expr -> Expr -> Expr
nMod x y = app "mod" [x,y]

nAbs :: Expr -> Expr
nAbs x = app "abs" [x]

nLeq :: Expr -> Expr -> Expr
nLeq x y = app "<=" [x,y]

nLt :: Expr -> Expr -> Expr
nLt x y = app "<" [x,y]

nGeq :: Expr -> Expr -> Expr
nGeq x y = app ">=" [x,y]

nGt :: Expr -> Expr -> Expr
nGt x y = app ">" [x,y]



example =
  Script
    [ CmdSetLogic (N "QF_LIA")
    , CmdDeclareFun (N "x") [] tInt
    , CmdDeclareFun (N "y") [] tInt
    , CmdDeclareFun (N "z") [] tBool
    , CmdAssert $ nGeq (app "x" []) (app "y" []) ==== (app "z" [])
    , CmdCheckSat
    , CmdGetValue [ app "x" [], app "y" [], app "z" [] ]
    ]




