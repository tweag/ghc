%
% Code generation for the Static Pointer Table
%
% (c) 2014 I/O Tweag
%
\begin{code}
module SPT (sptInitCode) where

import CoreSyn
import Module
import Outputable
import Id
import Name
import CLabel
import FastString
import Foreign.Ptr
import GHC.Fingerprint
import qualified Data.ByteString.Unsafe as BS
import System.IO.Unsafe (unsafePerformIO)
\end{code}

Each module that uses 'static' keyword declares an initialization
function of the form hs_spt_init_module() which is emitted into the _stub.c
file and annotated with __attribute__((constructor)) so that it gets
executed at startup time.

The function's purpose is to call hs_spt_module_init to insert the static
pointers of this module in the hashtable of the RTS, and it looks something
like this:

static void hs_hpc_init_Main(void) __attribute__((constructor));
static void hs_hpc_init_Main(void)
{
 extern StgPtr Main_sptEntryZC0_closure;
 extern StgPtr Main_sptEntryZC1_closure;
 hs_spt_module_init((void*[])
     { (StgWord64[2]){16252233376642134256ULL,7370534374097506082ULL}
     , &Main_sptEntryZC0_closure
     , (StgWord64[2]){12545634534567898323ULL,5409674567544156781ULL}
     , &Main_sptEntryZC1_closure
     , 0
     });
}

where constants are values of a fingerprint of the triplet
(package_id, module_name, sptEntry:N).

\begin{code}
sptInitCode :: Module -> [(Id,CoreExpr)] -> SDoc
sptInitCode _ [] = Outputable.empty
sptInitCode this_mod entries
  = vcat
    [ text "static void hs_spt_init_" <> ppr this_mod
           <> text "(void) __attribute__((constructor));"
    , text "static void hs_spt_init_" <> ppr this_mod <> text "(void)"
    , braces $ vcat $
        (map (\(n,_) ->
                 ptext (sLit "extern StgPtr ")
                 <> (ppr $ mkClosureLabel (idName n) (idCafInfo n))
                 <> semi)
            entries)
        ++ [ptext (sLit "hs_spt_module_init") <>
              parens (
                ptext (sLit "(void*[])")
                <> braces (hcat $ punctuate comma
                   ((concatMap (\(n,_) ->
                       [ pprFingerprint $ fingerprintId n
                       , ptext (sLit "&") <> ppr (mkClosureLabel (idName n) (idCafInfo n))
                       ])
                        entries)
                    ++ [ptext (sLit "0")]
                    ))
               ) <> semi]
    ]
\end{code}

\begin{code}
fingerprintId :: Id -> Fingerprint
fingerprintId n =
   fingerprintString (unpackFS name)
 where
   name = concatFS [ packageKeyFS $ modulePackageKey $ nameModule $ idName n
                   , fsLit ":"
                   , moduleNameFS (moduleName $ nameModule $ idName n)
                   , fsLit "."
                   , occNameFS $ occName $ idName n
                   ]

pprFingerprint :: Fingerprint -> SDoc
pprFingerprint (Fingerprint w1 w2) =
   ptext (sLit "(StgWord64[2])")
   <> (braces $ hcat $ punctuate comma [integer (fromIntegral w1) <> ptext  (sLit "ULL")
                                       ,integer (fromIntegral w2) <> ptext (sLit "ULL")
                                       ])
\end{code}

