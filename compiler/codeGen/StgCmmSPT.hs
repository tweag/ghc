-----------------------------------------------------------------------------
--
-- Code generation for the Static Pointer Table
--
-- (c) 2014 I/O Tweag
--
-----------------------------------------------------------------------------

module StgCmmSPT ( initSPT ) where

import CLabel
import CmmExpr
import FastString
import Id
import Module
import OccName
import StgCmmMonad
import StgCmmUtils


initSPT :: Module -> [Id] -> FCode ()
initSPT _ [] = return ()
-- Emit top-level tables for SPT entries
initSPT this_mod speIds = do
    emitDataLits (mkSPTLabel this_mod) . concat =<< sequence
      [ do key <- newStringCLit $ unpackFS $ concatFS
                    [ moduleNameFS (moduleName this_mod)
                    , fsLit "."
                    , occNameFS $ occName n
                    , fsLit "\0"
                    ]
           return [ key -- char* to "<module>.speEntry:<#>"
                  , CmmLabel $ mkClosureLabel n (idCafInfo speId)
                  ]
      | speId <- speIds, let n = idName speId
      ]
