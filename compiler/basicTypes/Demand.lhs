%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: A decoupled implementation of a demand domain}

\begin{code}

module Demand (
        LatticeLike, top, bot, lub, both, pre,
        StrDmd(..), strBot, strTop, strStr, strProd, strCall,
        AbsDmd(..), Count(..),
        Demand, JointDmd(..), mkJointDmd, mkProdDmd, 
        isTop, isBot, isAbs, absDmd,
	DmdType(..), topDmdType, botDmdType, mkDmdType, mkTopDmdType, 
		dmdTypeDepth, 
	DmdEnv, emptyDmdEnv,
	DmdResult(..), CPRResult(..), PureResult(..), 
        isBotRes, isTopRes, resTypeArgDmd, 
        topRes, botRes, cprRes,
        appIsBottom, isBottomingSig, pprIfaceStrictSig, returnsCPR, 
	StrictSig(..), mkStrictSig, topSig, botSig, cprSig,
        isTopSig, splitStrictSig, increaseStrictSigArity,
       
        seqStrDmd, seqStrDmdList, seqAbsDmd, seqAbsDmdList,
        seqDemand, seqDemandList, seqDmdType, seqStrictSig, 
        evalDmd, onceEvalDmd, vanillaCall, isStrictDmd, splitCallDmd, splitDmdTy,
        someCompUsed, isUsed, isUsedDmd,
        defer, use, deferType, deferEnv, modifyEnv,
        isProdDmd, isPolyDmd, replicateDmd, splitProdDmd, peelCallDmd, mkCallDmd,
        isProdUsage, 
        -- cardinality stuff
        markAsUsedType, isSingleUsed
     ) where

#include "HsVersions.h"

import StaticFlags
import Outputable
import VarEnv
import UniqFM
import Util
import BasicTypes
import Binary
import Maybes		         ( expectJust )

{-! for StrDmd derive: Binary !-}
{-! for AbsDmd derive: Binary !-}
{-! for Demand derive: Binary !-}
{-! for DmdResult derive: Binary !-}
{-! for DmdType derive: Binary !-}
{-! for StrictSig derive: Binary !-}

\end{code}

%************************************************************************
%*									*
\subsection{Lattice-like structure for domains}
%*									*
%************************************************************************

\begin{code}

class LatticeLike a where
  bot    :: a
  top    :: a
  pre    :: a -> a -> Bool
  lub    :: a -> a -> a 
  both   :: a -> a -> a

-- False < True
instance LatticeLike Bool where
  bot     = False
  top     = True
-- x `pre` y <==> (x => y)
  pre x y = (not x) || y  
  lub     = (||)
  both    = (&&)

\end{code}


%************************************************************************
%*									*
\subsection{Strictness domain}
%*									*
%************************************************************************

\begin{code}

-- Vanilla strictness domain
data StrDmd
  = HyperStr             -- Hyper-strict 
  | Lazy                 -- Lazy
  | SCall StrDmd         -- Call demand
  | Str                  -- Head-Strict
  | SProd [StrDmd]       -- Possibly deferred roduct or function demand
  deriving ( Eq, Show )

-- Well-formedness preserving constructors for the Strictness domain
strBot, strTop, strStr :: StrDmd
strBot     = HyperStr
strTop     = Lazy
strStr     = Str

strCall :: StrDmd -> StrDmd
strCall Lazy     = Lazy
strCall HyperStr = HyperStr
strCall s        = SCall s

strProd :: [StrDmd] -> StrDmd
strProd sx
  | any (== HyperStr) sx    = strBot
  | all (== Lazy) sx        = strStr
  | otherwise               = SProd sx

isStrict :: StrDmd -> Bool
isStrict (SCall s)   = isStrict s
isStrict Lazy        = False
isStrict _           = True

-- Pretty-printing
instance Outputable StrDmd where
  ppr HyperStr      = char 'B'
  ppr Lazy          = char 'L'
  ppr (SCall s)     = char 'C' <> parens (ppr s)
  ppr Str           = char 'S'
  ppr (SProd sx)    = char 'S' <> parens (hcat (map ppr sx))

-- LatticeLike implementation for strictness demands
instance LatticeLike StrDmd where
  bot = HyperStr
  top = Lazy
  
  pre _ Lazy                               = True
  pre HyperStr _                           = True
  pre (SCall s1) (SCall s2)                = pre s1 s2
  pre (SCall _) Str                        = True
  pre (SProd _) Str                        = True
  pre (SProd sx1) (SProd sx2)    
            | length sx1 == length sx2     = all (== True) $ zipWith pre sx1 sx2 
  pre x y                                  = x == y

  lub x y | x == y                         = x 
  lub y x | x `pre` y                      = lub x y
  lub HyperStr s                           = s
  lub _ Lazy                               = strTop
  lub (SProd _) Str                        = strStr
  lub (SProd sx1) (SProd sx2) 
           | length sx1 == length sx2      = strProd $ zipWith lub sx1 sx2
           | otherwise                     = strStr
  lub (SCall s1) (SCall s2)                = strCall (s1 `lub` s2)
  lub (SCall _)  Str                       = strStr
  lub _ _                                  = strTop

  both x y | x == y                        = x 
  both y x | x `pre` y                     = both x y
  both HyperStr _                          = strBot
  both s Lazy                              = s
  both s@(SProd _) Str                     = s
  both (SProd sx1) (SProd sx2) 
           | length sx1 == length sx2      = strProd $ zipWith both sx1 sx2 
  both (SCall s1) (SCall s2)               = strCall (s1 `both` s2)
  both s@(SCall _)  Str                    = s
  both _ _                                 = strBot

-- utility functions to deal with memory leaks
seqStrDmd :: StrDmd -> ()
seqStrDmd (SProd ds)   = seqStrDmdList ds
seqStrDmd (SCall s)     = s `seq` () 
seqStrDmd _            = ()

seqStrDmdList :: [StrDmd] -> ()
seqStrDmdList [] = ()
seqStrDmdList (d:ds) = seqStrDmd d `seq` seqStrDmdList ds

-- Serialization
instance Binary StrDmd where
  put_ bh HyperStr     = do putByte bh 0
  put_ bh Lazy         = do putByte bh 1
  put_ bh Str          = do putByte bh 2
  put_ bh (SCall s)    = do putByte bh 3
                            put_ bh s
  put_ bh (SProd sx)   = do putByte bh 4
                            put_ bh sx  
  get bh = do 
         h <- getByte bh
         case h of
           0 -> do return strBot
           1 -> do return strTop
           2 -> do return strStr
           3 -> do s  <- get bh
                   return $ strCall s
           _ -> do sx <- get bh
                   return $ strProd sx

-- Splitting polymorphic demands
replicateStrDmd :: Int -> StrDmd -> [StrDmd]
replicateStrDmd n Lazy         = replicate n Lazy
replicateStrDmd n HyperStr     = replicate n HyperStr
replicateStrDmd n Str          = replicate n Lazy
replicateStrDmd _ d            = pprPanic "replicateStrDmd" (ppr d)          

isPolyStrDmd :: StrDmd -> Bool
isPolyStrDmd Lazy     = True
isPolyStrDmd HyperStr = True
isPolyStrDmd Str      = True
isPolyStrDmd _        = False

\end{code}

%************************************************************************
%*									*
\subsection{Absence domain}
%*									*
%************************************************************************

\begin{code}

data AbsDmd
  = Abs                       -- Definitely unused
  | Used  Count               -- May be used
  | UCall Count AbsDmd        -- Call demand for absence
  | UHead Count               -- U(A...A)
  | UProd Count [AbsDmd]      -- Product [Invariant] not all components are absent
  deriving ( Eq, Show )

-- Absrtact counting of usages
data Count = One | Many
  deriving ( Eq, Show )     

-- Pretty-printing
instance Outputable AbsDmd where
  ppr Abs          = char 'A'
  ppr (Used c)     = char 'U' <> ppr c
  ppr (UCall c a)  = char 'C' <> ppr c <> parens (ppr a)
  ppr (UHead c)    = char 'H' <> ppr c
  ppr (UProd c as) = (char 'U') <> ppr c <> parens (hcat (map ppr as))

instance Outputable Count where
  ppr One  = char '1'
  ppr Many = text ""

-- Well-formedness preserving constructors for the Absence domain
usedMany :: AbsDmd
usedMany = (Used Many)

isUsedOnce :: AbsDmd -> Bool
isUsedOnce Abs           = True
isUsedOnce a 
         | One <- card a = True
isUsedOnce _             = False

absCall :: Count -> AbsDmd -> AbsDmd
absCall _ Abs  = Abs 
absCall c a    = UCall c a

absProd :: Count -> [AbsDmd] -> AbsDmd
absProd c ux 
  | all (== Abs) ux    = UHead c
  | otherwise          = UProd c ux


instance LatticeLike Count where
  bot        = One
  top        = Many
  pre x y    = (x == y) || (y == top)

  lub _ Many = Many
  lub Many _ = Many
  lub x _    = x 

  both _ _   = Many 

instance LatticeLike AbsDmd where
  bot                             = Abs
  top                             = Used Many
 
  pre Abs _                       = True
  pre (Used c1) (Used c2)         = pre c1 c2
  pre _ (Used _)                  = True
  pre (UHead c1) (UHead c2)       = pre c1 c2
  pre (UHead c1) (UCall c2 _)     = pre c1 c2
  -- for `seq`
  pre (UHead c1) (UProd c2 _)     = pre c1 c2
  pre (UCall c1 u1) (UCall c2 u2) = (pre c1 c2) && (pre u1 u2)
  pre (UProd c1 ux1) (UProd c2 ux2)
     | length ux1 == length ux2   = (pre c1 c2) && 
                                    (all (== True) $ zipWith pre ux1 ux2)
  pre x y                         = x == y

  lub x y | x == y                = x 
  lub y x | x `pre` y             = lub x y
  lub Abs a                       = a
  lub (Used c1) (Used c2)         = Used $ lub c1 c2
  lub a (Used c)                  = Used $ lub c (card a)
  lub (UHead c1) (UHead c2)       = UHead $ lub c1 c2
  lub (UHead c1) (UCall c2 u)     = UCall (lub c1 c2) u
  lub (UHead c1) (UProd c2 ux)    = UProd (c1 `lub` c2) ux
  lub (UProd c1 ux1) (UProd c2 ux2)
     | length ux1 == length ux2   = absProd (c1 `lub` c2) $ zipWith lub ux1 ux2
  lub (UCall c1 u1) (UCall c2 u2) = absCall (c1 `lub` c2) (u1 `lub` u2)
  lub _ _                         = top

  -- `both` is different from `lub` in its treatment of counting if
  -- `both` is computed for two used, the result always has
  -- cardinality `Many` (except for the Call demand -- [TODO] explain).  
  -- Also, `both` is not idempotent.

  both y x | x /= y && x `pre` y   = both x y
  both Abs a                       = a
  both _ (Used _)                  = Used Many
  both (UHead _) (UHead _)         = UHead Many
  both (UHead _) (UCall _ u)       = UCall Many u
  both (UHead _) (UProd _ ux)      = UProd Many ux
  both (UProd _ ux1) (UProd _ ux2)
     | length ux1 == length ux2    = absProd Many $ zipWith both ux1 ux2
  both (UCall _ u1) (UCall _ u2)   = absCall Many (u1 `lub` u2)
  both _ _                         = top

-- utility functions
card :: AbsDmd -> Count
card Abs         = pprPanic "count for Abs is invoked" (ppr Abs)
card (Used c)    = c
card (UHead c)   = c
card (UProd c _) = c
card (UCall c _) = c

markAsUsed :: AbsDmd -> AbsDmd
markAsUsed Abs         = Abs
markAsUsed (Used _)    = Used Many
markAsUsed (UHead _)   = UHead Many
markAsUsed (UProd _ x) = UProd Many $ map markAsUsed x
markAsUsed (UCall _ x) = markAsUsed x

seqAbsDmd :: AbsDmd -> ()
seqAbsDmd (Used c)     = c `seq` ()
seqAbsDmd (UHead c)    = c `seq` ()
seqAbsDmd (UProd c ds) = c `seq` seqAbsDmdList ds
seqAbsDmd (UCall c d)  = c `seq` seqAbsDmd d
seqAbsDmd _            = ()

seqAbsDmdList :: [AbsDmd] -> ()
seqAbsDmdList [] = ()
seqAbsDmdList (d:ds) = seqAbsDmd d `seq` seqAbsDmdList ds

-- Serialization
instance Binary Count where
    put_ bh One  = do putByte bh 0
    put_ bh Many = do putByte bh 1
    
    get  bh = do h <- getByte bh
                 case h of
                   0 -> return One
                   _ -> return Many   

instance Binary AbsDmd where
    put_ bh Abs          = do 
            putByte bh 0
    put_ bh (Used c)     = do 
            putByte bh 1
            put_ bh c
    put_ bh (UHead c)    = do 
            putByte bh 2
            put_ bh c
    put_ bh (UCall c u)  = do
            putByte bh 3
            put_ bh c
            put_ bh u
    put_ bh (UProd c ux) = do
            putByte bh 4
            put_ bh c
            put_ bh ux

    get  bh = do
            h <- getByte bh
            case h of 
              0 -> return Abs       
              1 -> do c  <- get bh
                      return $ Used c
              2 -> do c  <- get bh
                      return $ UHead c
              3 -> do c  <- get bh
                      u  <- get bh
                      return $ absCall c u  
              _ -> do c  <- get bh
                      ux <- get bh
                      return $ absProd c ux

-- Splitting polymorphic demands
replicateAbsDmd :: Int -> AbsDmd -> [AbsDmd]
replicateAbsDmd n Abs          = replicate n Abs
replicateAbsDmd n (Used _)     = replicate n (Used Many)
replicateAbsDmd n (UHead _)    = replicate n Abs
replicateAbsDmd _ d            = pprPanic "replicateAbsDmd" (ppr d)          

isPolyAbsDmd :: AbsDmd -> Bool
isPolyAbsDmd Abs         = True
isPolyAbsDmd (Used _)    = True
isPolyAbsDmd (UHead _)   = True
isPolyAbsDmd _           = False

\end{code}
  
%************************************************************************
%*									*
\subsection{Joint domain for Strictness and Absence}
%*									*
%************************************************************************

\begin{code}

data JointDmd = JD { strd :: StrDmd, absd :: AbsDmd } 
  deriving ( Eq, Show )

-- Pretty-printing
instance Outputable JointDmd where
  ppr (JD {strd = s, absd = a}) = angleBrackets (ppr s <> char ',' <> ppr a)

-- Well-formedness preserving constructors for the joint domain
mkJointDmd :: StrDmd -> AbsDmd -> JointDmd
mkJointDmd s a 
 = case (s, a) of 
     (HyperStr, UProd c _) -> JD {strd = HyperStr, absd = Used c}
     _                     -> JD {strd = s, absd = a}

mkProdDmd :: [JointDmd] -> JointDmd
mkProdDmd dx 
  = mkJointDmd sp up 
  where
    sp = strProd $ map strd dx
    up = absProd One $ map absd dx   
     
instance LatticeLike JointDmd where
  bot                        = mkJointDmd bot bot
  top                        = mkJointDmd top top

  pre x _ | x == bot         = True
  pre _ x | x == top         = True
  pre (JD {strd = s1, absd = a1}) 
      (JD {strd = s2, absd = a2})  = (pre s1 s2) && (pre a1 a2)

  lub  (JD {strd = s1, absd = a1}) 
       (JD {strd = s2, absd = a2}) = mkJointDmd (lub s1 s2)  $ lub a1 a2            
  both (JD {strd = s1, absd = a1}) 
       (JD {strd = s2, absd = a2}) = mkJointDmd (both s1 s2) $ both a1 a2            

isTop :: JointDmd -> Bool
isTop (JD {strd = Lazy, absd = Used Many}) = True
isTop _                                    = False 

isBot :: JointDmd -> Bool
isBot (JD {strd = HyperStr, absd = Abs}) = True
isBot _                                  = False 
  
isAbs :: JointDmd -> Bool
isAbs (JD {strd = Lazy, absd = Abs}) = True
isAbs _                              = False 

absDmd :: JointDmd
absDmd = mkJointDmd top bot 

-- More utility functions for strictness
seqDemand :: JointDmd -> ()
seqDemand (JD {strd = x, absd = y}) = x `seq` y `seq` ()

seqDemandList :: [JointDmd] -> ()
seqDemandList [] = ()
seqDemandList (d:ds) = seqDemand d `seq` seqDemandList ds

-- Serialization
instance Binary JointDmd where
    put_ bh (JD {strd = x, absd = y}) = do put_ bh x; put_ bh y
    get  bh = do 
              x <- get bh
              y <- get bh
              return $ mkJointDmd x y

isStrictDmd :: Demand -> Bool
isStrictDmd (JD {strd = x}) = isStrict x

isProdUsage :: Demand -> Bool
isProdUsage (JD {absd = (UProd _ _)}) = True
isProdUsage (JD {absd = Used _})      = True
isProdUsage _                         = False

isUsedDmd :: Demand -> Bool
isUsedDmd (JD {absd = x}) = x /= bot

isUsed :: AbsDmd -> Bool
isUsed x = x /= bot

someCompUsed :: AbsDmd -> Bool
someCompUsed (Used _)    = True
someCompUsed (UProd _ _) = True
someCompUsed _           = False

-- Evaluation demands
evalDmd :: JointDmd
evalDmd = mkJointDmd strStr top

onceEvalDmd :: JointDmd
onceEvalDmd = mkJointDmd strStr (Used One)

defer :: Demand -> Demand
defer (JD {absd = a}) = mkJointDmd top a 

use :: Demand -> Demand
use (JD {strd=d, absd=a}) = mkJointDmd d (markAsUsed a)

\end{code}

Note [Dealing with call demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Call demands are constructed and deconstructed coherently for
strictness and absence. For instance, the strictness signature for the
following function

f :: (Int -> (Int, Int)) -> (Int, Bool)
f g = (snd (g 3), True)

should be: <L,C(U(AU))>m

\begin{code}

mkCallDmd :: JointDmd -> JointDmd
mkCallDmd (JD {strd = d, absd = a}) 
          = mkJointDmd (strCall d) (absCall One a)

-- Returns result demand + one-shotness of the call
peelCallDmd :: JointDmd -> Maybe (JointDmd, Count)
peelCallDmd (JD {strd = SCall d, absd = UCall c a})  = Just (mkJointDmd d a, c)
peelCallDmd (JD {strd = Lazy, absd = UCall c a})     = Just (mkJointDmd Lazy a, c)
peelCallDmd (JD {strd = HyperStr, absd = UCall c a}) = Just (mkJointDmd HyperStr a, c)
peelCallDmd (JD {strd = SCall d, absd = Used _})     = Just (mkJointDmd d top, Many)
peelCallDmd _                                        = Nothing 


splitCallDmd :: JointDmd -> (Int, JointDmd)
splitCallDmd (JD {strd = SCall d, absd = UCall _ a}) 
  = case splitCallDmd (mkJointDmd d a) of
      (n, r) -> (n + 1, r)
-- Exploiting the fact that C(U) === U
splitCallDmd (JD {strd = SCall d, absd = Used _}) 
  = case splitCallDmd (mkJointDmd d top) of
      (n, r) -> (n + 1, r)
splitCallDmd d	      = (0, d)

isSingleUsed :: JointDmd -> Bool
isSingleUsed (JD {absd=a}) = isUsedOnce a

-- see Note [Default semands for right-hand sides]  
vanillaCall :: Arity -> Demand
vanillaCall 0 = onceEvalDmd
-- generate C^n (U)  
vanillaCall n =
  let strComp = (iterate strCall strStr) !! n
      absComp = (iterate (absCall Many) usedMany) !! n
   in mkJointDmd strComp absComp

\end{code}

Note [Default demands for right-hand sides]  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When analysis a right-hand side of a let binding, we create a
"default" demand using `vanillaCall`. It is owth mentioning that for
*thunks* the demand, under which a RHS is analysed is (Used One),
whereas for lambdas it is C(C...(U)...).

This fenomenon is due to the special nature of thunks: they "merge"
multiple usage demands into one. This also explains the fact that the
demand transformer for thunks is triggered by a less-precise, mere U
demand (not U1). This is not true for lambda, therefore to analyze
them we create a conservative demand C(C...(U)...), where the number
of call layers is equal to syntactic arity of the lambda.


Note [Replicating polymorphic demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some demands can be considered as polymorphic. Generally, it is
applicable to such beasts as tops, bottoms as well as Head-Used adn
Head-stricts demands. For instance,

S ~ S(L, ..., L)

Also, when top or bottom is occurred as a result demand, it in fact
can be expanded to saturate a callee's arity. 


\begin{code}

-- Replicating polymorphic demands
replicateDmd :: Int -> Demand -> [Demand]
replicateDmd _ d
  | not $ isPolyDmd d   = pprPanic "replicateDmd" (ppr d)          
replicateDmd n (JD {strd=x, absd=y}) = zipWith mkJointDmd (replicateStrDmd n x) 
                                                         (replicateAbsDmd n y)

-- Check whether is a product demand
isProdDmd :: Demand -> Bool
isProdDmd (JD {strd = SProd _, absd = UProd _ _})  = True
isProdDmd (JD {strd = SProd _, absd = a})          = isPolyAbsDmd a
isProdDmd (JD {strd = s, absd = UProd _ _})        = isPolyStrDmd s
isProdDmd _                                        = False

isPolyDmd :: Demand -> Bool
isPolyDmd (JD {strd=a, absd=b}) = isPolyStrDmd a && isPolyAbsDmd b

-- Split a product to parameteres
splitProdDmd :: Demand -> [Demand]
splitProdDmd JD {strd=SProd sx, absd=UProd _ ux}
  = ASSERT( sx `lengthIs` (length ux) ) zipWith mkJointDmd sx ux
splitProdDmd JD {strd=SProd sx, absd=u} 
  | isPolyAbsDmd u  
  =  zipWith mkJointDmd sx (replicateAbsDmd (length sx) u)
splitProdDmd (JD {strd=s, absd=UProd _ ux})
  | isPolyStrDmd s  
  =  zipWith mkJointDmd (replicateStrDmd (length ux) s) ux
splitProdDmd d = pprPanic "splitProdDmd" (ppr d)

\end{code}

%************************************************************************
%*									*
\subsection{Demand results}
%*									*
%************************************************************************

\begin{code}

------------------------------------------------------------------------
-- Pure demand result                                             
------------------------------------------------------------------------

data PureResult = TopRes	-- Nothing known, assumed to be just lazy
                | BotRes        -- Diverges or errors
 	       deriving( Eq, Show )

instance LatticeLike PureResult where
     bot = BotRes
     top = TopRes
     pre x y = (x == y) || (y == top)
     lub x y | x == y = x 
     lub _ _          = top
     both x y | x == y = x 
     both _ _          = bot

instance Binary PureResult where
    put_ bh BotRes       = do putByte bh 0
    put_ bh TopRes       = do putByte bh 1

    get  bh = do
            h <- getByte bh
            case h of 
              0 -> return bot       
              _ -> return top


------------------------------------------------------------------------
-- Constructed Product Result                                             
------------------------------------------------------------------------

data CPRResult = NoCPR
               | RetCPR
               deriving( Eq, Show )

instance LatticeLike CPRResult where
     bot = RetCPR
     top = NoCPR
     pre x y = (x == y) || (y == top)
     lub x y | x == y  = x 
     lub _ _           = top
     both x y | x == y = x 
     both _ _          = bot

instance Binary CPRResult where
    put_ bh RetCPR       = do putByte bh 0
    put_ bh NoCPR        = do putByte bh 1

    get  bh = do
            h <- getByte bh
            case h of 
              0 -> return bot       
              _ -> return top

------------------------------------------------------------------------
-- Combined demand result                                             --
------------------------------------------------------------------------

data DmdResult = DR { res :: PureResult, cpr :: CPRResult }
     deriving ( Eq )

-- TODO rework DmdResult to make it more clear
instance LatticeLike DmdResult where
  bot                        = botRes
  top                        = topRes

  pre x _ | x == bot         = True
  pre _ x | x == top         = True
  pre (DR s1 a1) (DR s2 a2)  = (pre s1 s2) && (pre a1 a2)

  lub  r r' | isBotRes r                   = r'
  lub  r r' | isBotRes r'                  = r
  lub  r r' 
        | returnsCPR r && returnsCPR r'    = r
  lub  _ _                                 = top

  both _ r | isBotRes r = r
  both r _              = r

-- Pretty-printing
instance Outputable DmdResult where
  ppr (DR {res=TopRes, cpr=RetCPR}) = char 'm'   --    DDDr without ambiguity
  ppr (DR {res=BotRes}) = char 'b'   
  ppr _ = empty	  -- Keep these distinct from Demand letters

instance Binary DmdResult where
    put_ bh (DR {res=x, cpr=y}) = do put_ bh x; put_ bh y
    get  bh = do 
              x <- get bh
              y <- get bh
              return $ mkDmdResult x y

mkDmdResult :: PureResult -> CPRResult -> DmdResult
mkDmdResult BotRes RetCPR = botRes
mkDmdResult x y = DR {res=x, cpr=y}

seqDmdResult :: DmdResult -> ()
seqDmdResult (DR {res=x, cpr=y}) = x `seq` y `seq` ()

-- [cprRes] lets us switch off CPR analysis
-- by making sure that everything uses TopRes instead of RetCPR
-- Assuming, of course, that they don't mention RetCPR by name.
-- They should onlyu use retCPR
topRes, botRes, cprRes :: DmdResult
topRes = mkDmdResult TopRes NoCPR
botRes = mkDmdResult BotRes NoCPR
cprRes | opt_CprOff = topRes
       | otherwise  = mkDmdResult TopRes RetCPR

isTopRes :: DmdResult -> Bool
isTopRes (DR {res=TopRes, cpr=NoCPR})  = True
isTopRes _                  = False

isBotRes :: DmdResult -> Bool
isBotRes (DR {res=BotRes})      = True
isBotRes _                  = False

returnsCPR :: DmdResult -> Bool
returnsCPR (DR {res=TopRes, cpr=RetCPR}) = True
returnsCPR _                  = False

resTypeArgDmd :: DmdResult -> Demand
-- TopRes and BotRes are polymorphic, so that
--	BotRes === Bot -> BotRes === ...
--	TopRes === Top -> TopRes === ...
-- This function makes that concrete
resTypeArgDmd r | isBotRes r = bot
resTypeArgDmd _              = top

\end{code}

%************************************************************************
%*									*
\subsection{Demand environments and types}
%*									*
%************************************************************************

\begin{code}


type Demand = JointDmd

type DmdEnv = VarEnv Demand

data DmdType = DmdType 
		  DmdEnv	-- Demand on explicitly-mentioned 
		       	        --	free variables
		  [Demand]	-- Demand on arguments
		  DmdResult	-- Nature of result
\end{code}

Note [Nature of result demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We assume the result in a demand type to be either a top or bottom
in order to represent the information about demand on the function
result, imposed by its definition. There are not so many things we 
can say, though. 

For instance, one can consider a function

        h = \v -> error "urk"

Taking the definition of strictness, we can easily see that 

        h undefined = undefined

that is, h is strict in v. However, v is not used somehow in the body
of h How can we know that h is strict in v? In fact, we know it by
considering a result demand of error and bottom and unleashing it on
all the variables in scope at a call site (in this case, this is only
v). We can also consider a case

       h = \v -> f x

where we know that the result of f is not hyper-strict (i.e, it is
lazy, or top). So, we put the same demand on v, which allow us to
infer a lazy demand that h puts on v.


\begin{code}
-- Equality needed for fixpoints in DmdAnal
instance Eq DmdType where
  (==) (DmdType fv1 ds1 res1)
       (DmdType fv2 ds2 res2) =  ufmToList fv1 == ufmToList fv2
			      && ds1 == ds2 && res1 == res2

instance LatticeLike DmdType where
  bot = botDmdType
  top = topDmdType

  pre (DmdType _ ds1 res1) (DmdType _ ds2 res2)
      = (res1 `pre` res2) &&
        (length ds1 == length ds2) &&
        all (\(x, y) -> x `pre` y) (zip ds1 ds2)

  lub (DmdType fv1 ds1 r1) (DmdType fv2 ds2 r2)
    = DmdType lub_fv2 (lub_ds ds1 ds2) (r1 `lub` r2)
    where
      absLub  = lub absDmd
      lub_fv  = plusVarEnv_C lub fv1 fv2
      -- Consider (if x then y else []) with demand V
      -- Then the first branch gives {y->V} and the second
      -- *implicitly* has {y->A}.  So we must put {y->(V `lub` A)}
      -- in the result env.
      lub_fv1 = modifyEnv (not (isBotRes r1)) absLub fv2 fv1 lub_fv
      lub_fv2 = modifyEnv (not (isBotRes r2)) absLub fv1 fv2 lub_fv1
	-- lub is the identity for Bot

	-- Extend the shorter argument list to match the longer
      lub_ds (d1:ds1) (d2:ds2) = lub d1 d2 : lub_ds ds1 ds2
      lub_ds []	    []	     = []
      lub_ds ds1    []	     = map (`lub` resTypeArgDmd r2) ds1
      lub_ds []	    ds2	     = map (resTypeArgDmd r1 `lub`) ds2
 
  both (DmdType fv1 ds1 r1) (DmdType fv2 _ r2)
    = DmdType both_fv2 ds1 (r1 `both` r2)
    where
      both_fv  = plusVarEnv_C both fv1 fv2
      both_fv1 = modifyEnv (isBotRes r1) (`both` bot) fv2 fv1 both_fv
      both_fv2 = modifyEnv (isBotRes r2) (`both` bot) fv1 fv2 both_fv1


instance Outputable DmdType where
  ppr (DmdType fv ds res) 
    = hsep [text "DmdType",
	    hcat (map ppr ds) <> ppr res,
	    if null fv_elts then empty
	    else braces (fsep (map pp_elt fv_elts))]
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd
      fv_elts = ufmToList fv

instance Binary DmdType where
  -- Ignore DmdEnv when spitting out the DmdType
  put_ bh (DmdType _ ds dr) 
       = do put_ bh ds 
            put_ bh dr
  get bh 
      = do ds <- get bh 
           dr <- get bh 
           return (DmdType emptyDmdEnv ds dr)

emptyDmdEnv :: VarEnv Demand
emptyDmdEnv = emptyVarEnv

topDmdType, botDmdType, cprDmdType :: DmdType
topDmdType = DmdType emptyDmdEnv [] topRes
botDmdType = DmdType emptyDmdEnv [] botRes
cprDmdType = DmdType emptyDmdEnv [] cprRes

isTopDmdType :: DmdType -> Bool
isTopDmdType (DmdType env [] res)
             | isTopRes res && isEmptyVarEnv env = True
isTopDmdType _                                   = False

mkDmdType :: DmdEnv -> [Demand] -> DmdResult -> DmdType
mkDmdType fv ds res = DmdType fv ds res

mkTopDmdType :: [Demand] -> DmdResult -> DmdType
mkTopDmdType ds res = DmdType emptyDmdEnv ds res

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdType _ ds _) = length ds

seqDmdType :: DmdType -> ()
seqDmdType (DmdType _env ds res) = 
  {- ??? env `seq` -} seqDemandList ds `seq` seqDmdResult res `seq` ()

splitDmdTy :: DmdType -> (Demand, DmdType)
-- Split off one function argument
-- We already have a suitable demand on all
-- free vars, so no need to add more!
splitDmdTy (DmdType fv (dmd:dmds) res_ty) = (dmd, DmdType fv dmds res_ty)
splitDmdTy ty@(DmdType _ [] res_ty)       = (resTypeArgDmd res_ty, ty)

deferType :: DmdType -> DmdType
deferType (DmdType fv _ _) = DmdType (deferEnv fv) [] top

deferEnv :: DmdEnv -> DmdEnv
deferEnv fv = mapVarEnv defer fv

markAsUsedType :: DmdType -> DmdType
markAsUsedType (DmdType fv _ _) = DmdType (markAsUsedEnv fv) [] top

markAsUsedEnv :: DmdEnv -> DmdEnv
markAsUsedEnv fv = mapVarEnv use fv


modifyEnv :: Bool			-- No-op if False
	  -> (Demand -> Demand)		-- The zapper
	  -> DmdEnv -> DmdEnv		-- Env1 and Env2
	  -> DmdEnv -> DmdEnv		-- Transform this env
	-- Zap anything in Env1 but not in Env2
	-- Assume: dom(env) includes dom(Env1) and dom(Env2)
modifyEnv need_to_modify zapper env1 env2 env
  | need_to_modify = foldr zap env (varEnvKeys (env1 `minusUFM` env2))
  | otherwise	   = env
  where
    zap uniq env = addToUFM_Directly env uniq (zapper current_val)
		 where
		   current_val = expectJust "modifyEnv" (lookupUFM_Directly env uniq)

\end{code}

%************************************************************************
%*									*
\subsection{Demand signature}
%*									*
%************************************************************************

In a let-bound Id we record its strictness info.  
In principle, this strictness info is a demand transformer, mapping
a demand on the Id into a DmdType, which gives
	a) the free vars of the Id's value
	b) the Id's arguments
	c) an indication of the result of applying 
	   the Id to its arguments

However, in fact we store in the Id an extremely emascuated demand transfomer,
namely 
		a single DmdType
(Nevertheless we dignify StrictSig as a distinct type.)

This DmdType gives the demands unleashed by the Id when it is applied
to as many arguments as are given in by the arg demands in the DmdType.

If an Id is applied to less arguments than its arity, it means that
the demand on the function at a call site is weaker than the vanilla
call demand, used for signature inference. Therefore we place a top
demand on all arguments. Otherwise, the demand is specified by Id's
signature.

For example, the demand transformer described by the DmdType
		DmdType {x -> <S(LL),U(UU)>} [V,A] Top
says that when the function is applied to two arguments, it
unleashes demand <S(LL),U(UU)> on the free var x, V on the first arg,
and A on the second.  

If this same function is applied to one arg, all we can say is that it
uses x with <L,U>, and its arg with demand <L,U>.

\begin{code}
newtype StrictSig = StrictSig DmdType
		  deriving( Eq )

instance Outputable StrictSig where
   ppr (StrictSig ty) = ppr ty

mkStrictSig :: DmdType -> StrictSig
mkStrictSig dmd_ty = StrictSig dmd_ty

splitStrictSig :: StrictSig -> ([Demand], DmdResult)
splitStrictSig (StrictSig (DmdType _ dmds res)) = (dmds, res)

increaseStrictSigArity :: Int -> StrictSig -> StrictSig
-- Add extra arguments to a strictness signature
increaseStrictSigArity arity_increase (StrictSig (DmdType env dmds res))
  = StrictSig (DmdType env (replicate arity_increase top ++ dmds) res)

isTopSig :: StrictSig -> Bool
isTopSig (StrictSig ty) = isTopDmdType ty

isBottomingSig :: StrictSig -> Bool
isBottomingSig (StrictSig (DmdType _ _ res)) = isBotRes res

topSig, botSig, cprSig:: StrictSig
topSig = StrictSig topDmdType
botSig = StrictSig botDmdType
cprSig = StrictSig cprDmdType

-- Serialization
instance Binary StrictSig where
    put_ bh (StrictSig aa) = do
            put_ bh aa
    get bh = do
          aa <- get bh
          return (StrictSig aa)
	
\end{code}

Note [Non-full application] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

If a function having bottom as its demand result is applied to a less
number of arguments than its syntactic arity, we cannot say for sure
that it is going to diverge. This is the reason why we use the
function appIsBottom, which, given a strictness signature and a number
of arguments, says conservatively if the function is going to diverge
or not.

\begin{code}

-- appIsBottom returns true if an application to n args would diverge
appIsBottom :: StrictSig -> Int -> Bool
appIsBottom (StrictSig (DmdType _ ds res)) n
            | isBotRes res                      = not $ lengthExceeds ds n 
appIsBottom _				      _ = False

seqStrictSig :: StrictSig -> ()
seqStrictSig (StrictSig ty) = seqDmdType ty

-- Used for printing top-level strictness pragmas in interface files
pprIfaceStrictSig :: StrictSig -> SDoc
pprIfaceStrictSig (StrictSig (DmdType _ dmds res))
  = hcat (map ppr dmds) <> ppr res
\end{code}

