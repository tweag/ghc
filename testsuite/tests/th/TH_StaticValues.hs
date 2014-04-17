{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE StaticValues #-}

-- |A test to load symbols produced by the static form.
--
-- First we have this program load itself using the GHC API.
-- Then we look for the symbols that the static form should have
-- exposed and use the values found at the symbol addresses.
--
module Main(main) where

import GHC.Ptr          ( Ptr(..), nullPtr )
import Foreign.C.String ( withCString, CString )
import GHC.Exts         ( addrToAny# )
import GHC.Ref
import Language.Haskell.TH
import System.Info      ( os )
import System.Environment

import Control.Monad.IO.Class ( liftIO )
import DynFlags
import Encoding               ( zEncodeString )
import GHC

main = do {
    ; [libdir] <- getArgs
    ; runGhc (Just libdir) $ do
      oldFlags <- getDynFlags
      setSessionDynFlags oldFlags  {hscTarget = HscInterpreted, ghcLink = LinkInMemory , ghcMode = CompManager
                                   , packageFlags = [ ExposePackage "ghc" ]
                                   , ldInputs = FileOption "" "TH_StaticValues.o" : ldInputs oldFlags
                                   }
      load LoadAllTargets
      liftIO $ do
        unstaticMain $([| static g |]) >>= putStrLn
        -- The ((>>=) $ x) avoids the need to type long type signatures when
        -- using impredicative types and direct application (x >>=).
        ((>>=) $ unstaticMain $([| static (id . (+)) |])) $
          \op -> print $ op (2 :: Int) (1 :: Int)
        ((>>=) $ unstaticMain $([| static (id . show) |])) $
          \sh -> putStrLn $ sh (1 :: Int)
    }

  where
    unstaticMain :: Ref a -> IO a
    unstaticMain (Ref (GlobalName "main" "" m n)) =
      loadFunction__ m n
        >>= maybe (error $ m ++ "." ++ n ++ " not found") return
    unstaticMain gn = error $ "unexpected package in " ++ show gn

g = "hello"

loadFunction__ :: String
              -> String
              -> IO (Maybe a)
loadFunction__ m valsym = do
    let symbol = prefixUnderscore++zEncodeString m++"_"++(zEncodeString valsym)++"_closure"
    ptr@(Ptr addr) <- withCString symbol c_lookupSymbol
    if (ptr == nullPtr)
      then return Nothing
      else case addrToAny# addr of
             (# hval #) -> return ( Just hval )
  where
    prefixUnderscore = if elem os ["darwin","mingw32","cygwin"] then "_" else ""


foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)
