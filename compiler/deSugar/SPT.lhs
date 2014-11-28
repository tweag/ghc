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
import CLabel
import FastString
import GHC.Fingerprint
\end{code}

%************************************************************************
%*                                                                      *
%*              initialisation
%*                                                                      *
%************************************************************************

Each module that uses 'static' keyword declares an initialization
function of the form hs_spt_init_module() which is emitted into the _stub.c
file and annotated with __attribute__((constructor)) so that it gets
executed at startup time.

The function's purpose is to call hs_spt_module to register this
module with the RTS, and it looks something like this:

static void hs_hpc_init_Main(void) __attribute__((constructor));
static void hs_hpc_init_Main(void)
{
 extern StgWord64 Main_sptEntryZC0_closure[];
 ...
 hs_spt_module_init((uint64_t[]){16252233376642134256ULL,7370534374097506082ULL}, Main_sptEntryZC0_closure...});
}

where constants are values of a fingerprint of z-encoded version of
static point entry name, see 'GHC.Fingerprint' for details.

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
        ++ [ptext (sLit "hs_spt_module") <>
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
fingerprintId n = fingerprintString $ showSDocSimple (pprCode CStyle $ ppr $ idName n)

pprFingerprint :: Fingerprint -> SDoc
pprFingerprint (Fingerprint w1 w2) =
   ptext (sLit "(uint64_t[])")
   <> (braces $ hcat $ punctuate comma [integer (fromIntegral w1) <> ptext  (sLit "ULL")
                                       ,integer (fromIntegral w2) <> ptext (sLit "ULL")
                                       ])
\end{code}

