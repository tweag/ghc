-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StaticPtr
-- Copyright   :  (C) 2014 I/O Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Symbolic references to values.
--
-- References to values are usually implemented with memory addresses, and this
-- is practical when communicating values between the different pieces of a
-- single process.
--
-- When values are communicated across different processes running in possibly
-- different machines, though, addresses are no longer useful since each
-- process may use different addresses to store a given value.
--
-- To solve such concern, the references provided by this module indicate
-- package, module and name of a value. This information could be used to locate
-- the value in different processes.
--
-- Currently, the main use case for references is the StaticPointers language
-- extension.
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE ExistentialQuantification #-}
module GHC.StaticPtr
  ( StaticPtr(..)
  , StaticName(..)
  , DynStaticPtr(..)
  , SptEntry
  , deRefStaticPtr
  , encodeStaticPtr
  , decodeStaticPtr
  ) where

import Data.Typeable    (Typeable)
import Data.Char
import Foreign.C.String ( withCString, CString )
import Foreign.Marshal  ( withArray )
import Foreign.Ptr      ( castPtr )
import GHC.Exts         ( addrToAny# )
import GHC.Ptr          ( Ptr(..), nullPtr )
import GHC.Fingerprint  ( Fingerprint(..), fingerprintString )
import Numeric
import System.Info      ( os )
import System.IO.Unsafe ( unsafePerformIO )
import Unsafe.Coerce    ( unsafeCoerce )


-- | A reference to a top-level value of type 'a'.
--
-- TODO make this into a newtype.
data StaticPtr a = StaticPtr { unStaticPtr :: StaticName }
  deriving (Read, Show, Typeable)

-- | Identification of top-level values
--
-- > StaticName package_id module_name value_name
--
data StaticName = StaticName String String String
  deriving (Read, Show, Typeable)

-- | Entries of the static pointer table.
data SptEntry = forall a . SptEntry StaticName a

-- | Dynamic static pointer.
data DynStaticPtr = forall a . DSP (StaticPtr a)

-- | Encodes static pointer in the form that can be later serialized.
encodeStaticPtr :: StaticPtr a -> Fingerprint
encodeStaticPtr = fingerprintStaticName . unStaticPtr

-- | Decodes an encoded pointer. It looks up a static pointer in
-- entry in the static pointer table.
decodeStaticPtr :: Fingerprint -> Maybe DynStaticPtr
decodeStaticPtr key = unsafePerformIO $
   fmap (fmap (\(SptEntry s _) -> DSP $ StaticPtr s)) (sptLookup key)

-- | Dereferences a static pointer.
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr p@(StaticPtr s) = unsafePerformIO $ do
    sptLookup (fingerprintStaticName s) >>=
      maybe (error $ "Unknown StaticPtr: " ++ show p)
            (\(SptEntry _ a) -> return $ unsafeCoerce a)

fingerprintStaticName :: StaticName -> Fingerprint
fingerprintStaticName (StaticName pkg m valsym) =
    fingerprintString $ concat [pkg, ":", m, ".", valsym]

sptLookup :: Fingerprint -> IO (Maybe SptEntry)
sptLookup (Fingerprint w1 w2) = do
    ptr@(Ptr addr) <- withArray [w1,w2] (hs_spt_lookup . castPtr)
    if (ptr == nullPtr)
    then return Nothing
    else case addrToAny# addr of
           (# spe #) -> return (Just spe)

foreign import ccall unsafe hs_spt_lookup :: Ptr () -> IO (Ptr a)
