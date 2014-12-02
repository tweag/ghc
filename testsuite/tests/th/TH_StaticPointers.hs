{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StaticPointers #-}

-- |A test to load symbols produced by the static form.
--
-- First we have this program load itself using the GHC API.
-- Then we look for the symbols that the static form should have
-- exposed and use the values found at the symbol addresses.
--
-- Note that we lookup for 'g' in symbol tables which does not appear
-- in the export list of Main.
--
module Main(main) where

import GHC.StaticPtr

main = print $ deRefStaticPtr $([| static g :: StaticPtr String |])

g = "found"
