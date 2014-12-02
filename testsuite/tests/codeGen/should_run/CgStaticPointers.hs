{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers     #-}

-- |A test to load symbols produced by the static form.
--
-- First we have this program load itself using the GHC API.
-- Then we look for the symbols that the static form should have
-- exposed and use the values found at the symbol addresses.
--
module Main(main) where

import Data.Typeable
import GHC.StaticPtr

main :: IO ()
main = do
  -- For some reason, removing the type signature below causes @g@ to appear
  -- in the desugarer with a coercion like:
  -- main@main:Main.g{v r20J} |> (Sub cobox_a36d{v}[lid])
  print $ deRefStaticPtr (static g :: StaticPtr String)
  -- For some reason, removing the type signature below causes an assertion
  -- failure in the compiler:
  --
  -- ASSERT failed! file compiler/typecheck/TcType.lhs line 645
  print $ deRefStaticPtr (static t_field :: StaticPtr (T Char -> Char)) $ T 'b'

g :: String
g = "found"

data T a = T { t_field :: a }
  deriving Typeable
