-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Ref
-- Copyright   :  (C) 2014 EURL Tweag
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
-- Currently, the main use case for references is the StaticValues language
-- extension.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
module GHC.Ref where

import Data.Typeable    (Typeable)
import Encoding         ( zEncodeString )
import Foreign.C.String ( withCString, CString )
import GHC.Exts         ( addrToAny# )
import GHC.Ptr          ( Ptr(..), nullPtr )

-- | A reference to a top-level value of type 'a'.
data Ref a = Ref { unRef :: GlobalName }
  deriving (Show, Typeable)

-- | Global names identifying top-level values
--
-- > GlobalName package_id installed_package_id module_name value_name
--
-- In essence, a 'GlobalName' augments the information provided by
-- 'Language.Haskell.TH.Syntax.Name' with the information in the
-- @installed_package_id@ field. This field is
-- Cabal:'Distribution.Package.InstalledPackageId' and it
-- is needed to identify the package when multiple variations of it are
-- installed.
--
data GlobalName = GlobalName String String String String
  deriving (Show, Typeable)

-- | An unsafe lookup function for symbolic references.
--
-- @deRef (r :: Ref a)@ returns @Nothing@ if no associated value is found for
-- @r@, and @Just v@ if @v@ is the value associated to @r@ and it has the
-- expected type @a@.
--
-- This function is unsafe because if an associated value @v@ of a different
-- type is found, then the behavior of this function is undefined.
--
-- Currently, a value is considered associated to a reference if the module
-- producing the reference has been dinamically linked to the program which
-- calls deRef.
--
deRef :: Ref a -> IO (Maybe a)
deRef (Ref (GlobalName pkg _ m n)) = do
    let mpkg = case pkg of
                 "main" -> Nothing
                 _ -> Just pkg
    loadFunction mpkg m n

-- loadFunction__ taken from
-- @plugins-1.5.4.0:System.Plugins.Load.loadFunction__@
loadFunction :: Maybe String
             -> String
             -> String
             -> IO (Maybe a)
loadFunction mpkg m valsym = do
    let symbol = prefixUnderscore
                   ++ maybe "" (\p -> zEncodeString p ++ "_") mpkg
                   ++ zEncodeString m ++ "_" ++ zEncodeString valsym
                   ++ "_closure"
    ptr@(Ptr addr) <- withCString symbol c_lookupSymbol
    if (ptr == nullPtr)
    then return Nothing
    else case addrToAny# addr of
           (# hval #) -> return ( Just hval )
  where
    prefixUnderscore = if elem os ["darwin","mingw32","cygwin"] then "_" else ""

foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)
