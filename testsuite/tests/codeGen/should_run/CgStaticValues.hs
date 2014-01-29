{-# LANGUAGE StaticValues #-}

-- |A test to load symbols produced by the static form.
--
-- First we have this program load itself using the GHC API.
-- Then we look for the symbols that the static form should have
-- exposed and use the values found at the symbol addresses.
--
module Main(main) where

import GHC.Ref
import System.Environment

import Control.Monad.IO.Class ( liftIO )
import DynFlags
import GHC

main = do {
    ; [libdir] <- getArgs
    ; runGhc (Just libdir) $ do
      oldFlags <- getDynFlags
      setSessionDynFlags oldFlags  {hscTarget = HscInterpreted, ghcLink = LinkInMemory , ghcMode = CompManager
                                   , packageFlags = [ ExposePackage "ghc" ]
                                   , ldInputs = FileOption "" "CgStaticValues.o" : ldInputs oldFlags
                                   }
      load LoadAllTargets
      liftIO $ do
        unstatic (static g) >>= putStrLn
        unstatic (f0 :: Ref Char) >>= print
        unstatic (f1 :: Ref Char) >>= print
        unstatic (static (id . id)) >>= \op ->
          print (op (1 :: Int))
    }

  where
    unstatic :: Ref a -> IO a
    unstatic r = deRef r >>= maybe (error $ show r ++ " not found") return

g = "hello"

g1 = '1'

class C a where
  f0 :: Ref a
  f1 :: Ref a

instance C Char where
  f0 = static g1
  f1 = static '2'
