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
import System.Environment

import Control.Monad.IO.Class ( liftIO )
import DynFlags
import GHC

import Unsafe.Coerce

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
    oldFlags <- getDynFlags
    setSessionDynFlags oldFlags
      { hscTarget    = HscInterpreted
      , ghcLink      = LinkInMemory
      , ghcMode      = CompManager
      , packageFlags = [ ExposePackage (PackageArg "ghc") Nothing ]
      , ldInputs     = FileOption "" "CgStaticValues.o" : ldInputs oldFlags
      }
    load LoadAllTargets
    liftIO $ do
      -- For some reason, removing the type signature below causes @g@ to appear
      -- in the desugarer with a coercion like:
      -- main@main:Main.g{v r20J} |> (Sub cobox_a36d{v}[lid])
      print $ deRefStaticPtr (static g :: StaticPtr String)
      -- For some reason, removing the type signature below causes an assertion
      -- failure in the compiler:
      --
      -- ASSERT failed! file compiler/typecheck/TcType.lhs line 645
      print $ deRefStaticPtr (static t_field :: StaticPtr (T Char -> Char)) $ T 'b'
      let s = (static g :: StaticPtr String)
          f = encodeStaticPtr s
      print f
      let (Just p) = decodeStaticPtr f
      print $ (\(DSP x) -> deRefStaticPtr (unsafeCoerce x :: StaticPtr String)) p

g :: String
g = "found"

data T a = T { t_field :: a }
  deriving Typeable
