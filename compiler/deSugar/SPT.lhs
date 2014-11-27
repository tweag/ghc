%
% Code generation for the Static Pointer Table
%
% (c) 2014 I/O Tweag
%
\begin{code}
{-# LANGUAGE NondecreasingIndentation #-}
{-# OPITIONS_GHC -Wall -Werror #-}

module SPT (hpcSptCode) where

import Module
import Outputable
import Id
import TyCon
import CLabel
import FastString
\end{code}

%************************************************************************
%*                                                                      *
%*              initialisation
%*                                                                      *
%************************************************************************

Each module that contains static keywork declares an initialization
function of the form hs_spt_init_module() which is emitted into the _stub.c
file and annotated with __attribute__((constructor)) so that it gets
executed at startup time.

The function's purpose is to call hs_spt_module to register this
module with the RTS, and it looks something like this:

static void hs_hpc_init_Main(void) __attribute__((constructor));
static void hs_hpc_init_Main(void)
{
 extern StgWord64 Main_sptEntryZC0_closure[];
 hs_spt_module_init({ "Main.sptEntry:0", Main_sptEntryZC0_closure });
}

\begin{code}
hpcSptCode :: Module -> [Id] -> SDoc
hpcSptCode _ [] = Outputable.empty
hpcSptCode this_mod entries 
 = vcat
    [ text "static void hs_spt_init_" <> ppr this_mod
         <> text "(void) __attribute__((constructor));"
    , text "static void hs_spt_init_" <> ppr this_mod <> text "(void)"
    , braces (vcat (
        (map (\n -> ptext (sLit "extern StgPtr ") <> (ppr $ mkClosureLabel (idName n) (idCafInfo n)) <> semi) 
            entries)
        ++ [ptext (sLit "hs_spt_module") <>
              parens (ptext (sLit "(void*[])") <>
                braces (hcat $ punctuate comma
                       ((concatMap (\n -> [doubleQuotes (ppr $ idName n), ppr $ mkClosureLabel (idName n) (idCafInfo n)])
                                   entries)
                        ++ [ptext (sLit "0")]
                       ))
		     ) <> semi]))
    ]
    {-
  where
    zencode id = zEncodeFS $ concatFS
       [ packageKeyFS $ modulePackageKey $ nameModule $ idName n
       , fsLit "_"
       , moduleNameFS (moduleName $ nameModule $ idName n)
       , fsLit "_"
       , occNameFS $ occName $ idName n
       ]
       where n = idName id
     -}
\end{code}
