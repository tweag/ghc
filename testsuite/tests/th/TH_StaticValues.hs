{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StaticValues #-}

-- |A test to load symbols produced by the static form.
--
-- First we have this program load itself using the GHC API.
-- Then we look for the symbols that the static form should have
-- exposed and use the values found at the symbol addresses.
--
module Main(main) where

import GHC.Ref
import Language.Haskell.TH
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
                                   , ldInputs = FileOption "" "TH_StaticValues.o" : ldInputs oldFlags
                                   }
      load LoadAllTargets
      liftIO $ do
        unstatic $([| static g |]) >>= putStrLn
        unstatic $([| static (id . id) |]) >>=
          \op -> print $ op (2 :: Int)
    }
  where
    unstatic :: Ref a -> IO a
    unstatic r = deRef r >>= maybe (error $ show r ++ " not found") return

g = "hello"

