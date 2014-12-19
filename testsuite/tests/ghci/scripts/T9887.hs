{-# LANGUAGE StaticPointers #-}
module T9887 where

import GHC.StaticPtr

f = deRefStaticPtr (static True)

